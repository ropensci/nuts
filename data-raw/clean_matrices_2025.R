# BUILD CONVERSION MATRICES BASED ON MATRICES PROVIDED BY THE EC's JRC

rm(list = ls())
require(tidyverse)
require(magrittr)
require(devtools)
require(rvest)

# Read section on data
# https://r-pkgs.org/data.html#sec-data-data


csv_files = list.files("data-raw/_JRC_NUTSconverter_Matrices_rel.2025.08", pattern="*.csv", full.names=TRUE)
#csv_files <- csv_files[1:5]

read_csv(csv_files[2])

# PATCHES FOR VERSION 2016, see ropensci/nuts#7
#-----------------------------------------------
# In the 2016 version of the tables of release 2025.08, two regions of the
# official NUTS 2016 classification (GISCO boundaries) are split in two:
#
# - Trøndelag (Norway, NUTS 3) is coded as NO061 and NO062 instead of NO060.
#   The two-region geometry also assigns the former municipality of Rissa
#   (about 621 km2) to NO062 instead of NO061, which creates a spurious flow
#   NO061 <-> NO062 between versions 2013 and 2016.
# - Northern Ireland (United Kingdom, NUTS 2) is coded as UKN0 and UKN1
#   instead of UKN0.
#
# Fix: recode the wrong codes in the 2016 code column and sum the flows of the
# rows that then share a pair of codes. Flows are additive, so this is exact.
# The csv files supplied by the JRC are left untouched.
patch_2016 <- function(d, old_codes, new_code) {
  flow_cols <- c("areaKm", "pop11", "pop21", "artif_surf12", "artif_surf18", "bu_vol")
  code_cols <- grep("^code_v\\d{4}$", names(d), value = TRUE)
  stopifnot(length(code_cols) == 2, all(flow_cols %in% names(d)))

  # Nothing to do for tables without version 2016 or without the region
  if (!"code_v2016" %in% code_cols || !any(d$code_v2016 %in% old_codes)) {
    return(d)
  }

  col_order <- names(d)
  out <- d %>%
    mutate(code_v2016 = if_else(code_v2016 %in% old_codes, new_code, code_v2016),
           .row = row_number()) %>%
    group_by(across(all_of(code_cols))) %>%
    summarise(across(all_of(flow_cols), sum), .row = min(.row), .groups = "drop") %>%
    mutate(uniqueID = paste0(.data[[code_cols[1]]], .data[[code_cols[2]]])) %>%
    # keep the original row order, so that only the patched rows differ
    arrange(.row) %>%
    select(all_of(col_order))

  # The patch must not create or destroy any area, population, etc.
  stopifnot(isTRUE(all.equal(colSums(d[flow_cols]), colSums(out[flow_cols]))),
            !any(out$code_v2016 %in% old_codes))
  out
}

cross_walks <- list()
n_patched <- c(NO = 0, UK = 0)
for( i in seq_along(csv_files) ){

  #i = 1

  # read conversion matrix supplied by the JRC
  t <- read_csv( csv_files[i] )

  # recode Trøndelag and Northern Ireland in version 2016 (ropensci/nuts#7)
  n_before <- nrow(t)
  t <- patch_2016(t, old_codes = c("NO061", "NO062"), new_code = "NO060")
  n_patched["NO"] <- n_patched["NO"] + (nrow(t) < n_before)
  n_before <- nrow(t)
  t <- patch_2016(t, old_codes = "UKN1", new_code = "UKN0")
  n_patched["UK"] <- n_patched["UK"] + (nrow(t) < n_before)
  from_year = str_extract( names(t)[1], "[0-9]{4}" )
  to_year = str_extract( names(t)[2], "[0-9]{4}" )
  # level = if (nchar(t[1, 1]) == 3) {
  #   1
  # } else if (nchar(t[1, 1]) == 4) {
  #   2
  # } else {
  #   3
  # }

  # create cross-walk forward in time with flows
  names( t )[ 1 ] <- 'from_code'
  names( t )[ 2 ] <- 'to_code'
  t <- t %>%
    mutate(
      from_year = from_year
      , to_year = to_year
      , from_version = from_year
      , to_version = to_year
    ) %>%
    # drop uniqueID since it's not unique with many classifications in one df
    select( from_code , to_code
            , from_version , to_version
            , areaKm , pop21 , pop11 , artif_surf18 , artif_surf12 , bu_vol
             )

  # create cross-walk backward in time with flows
  t2 <- t
  t2[ , c("from_code", "to_code")] <- t2[, c("to_code", "from_code")]
  t2[ , c("from_version", "to_version")] <- t2[, c("to_version", "from_version")]


  # create stand still for units needing no conversion with stocks
  # stocks in from year
  t3_a <- t %>%
    group_by( from_code, from_version) %>%
    summarise_at( vars( areaKm : bu_vol ) , ~sum( . )) %>%
    ungroup( ) %>%
    unique( ) %>%
    mutate( to_code = from_code
            , to_version = from_version) %>%
    select( colnames( t ))

  # stocks in to year
  t3_b <- t %>%
    group_by( to_code, to_version) %>%
    summarise_at( vars( areaKm : bu_vol ) , ~sum( . )) %>%
    ungroup( ) %>%
    unique( ) %>%
    mutate( from_code = to_code
            , from_version = to_version) %>%
    select( colnames( t ))

  t3 <- bind_rows(t3_a, t3_b)
  rm(t3_a, t3_b)

  # Bind all in one df
  t4 <- t %>% bind_rows( t2 ) %>% bind_rows( t3 )

  # from t=0 to t=1
  t %>% group_by(from_version, to_version) %>% tally()
  # from t=1 to t=0
  t2 %>% group_by(from_version, to_version) %>% tally()
  # from t=0 to t=0 and t=1 to t=1
  t3 %>% group_by(from_version, to_version) %>% tally()
  # everything
  t4 %>% group_by(from_version, to_version) %>% tally()

  # bind into list
  cross_walks[[i]] <- t4
}

# The Trøndelag patch applies to the five NUTS 3 tables that pair 2016 with
# 2006, 2010, 2013, 2021 and 2024. The Northern Ireland patch applies to the
# four NUTS 2 tables that pair 2016 with 2006, 2010, 2013 and 2021 (the United
# Kingdom is not part of version 2024).
stopifnot(n_patched["NO"] == 5, n_patched["UK"] == 4)

# maintain in one df at the from-to classification and from-to code level
cross_walks <- cross_walks %>%
  bind_rows() %>%
  select(from_code, to_code, from_version, to_version, names(.)) %>%
  distinct(from_code, to_code, from_version, to_version, .keep_all = T) %>%
  arrange(from_version, to_version, from_code, to_code)

# Prepare more identifiers
cross_walks <- cross_walks %>%
  mutate(level = case_when(
            nchar( from_code ) == 3 ~ 1
            , nchar( from_code ) == 4 ~ 2
            , nchar( from_code ) == 5 ~ 3
            , .default = NA
          )) %>%
  select(from_code, to_code, from_version, to_version,
         level, names(.))


# the cross walk should contain all classification year-year combinations
# 5 x 5
cross_walks %>%
  filter(nchar(from_code) == 3) %>%
  group_by(from_version, to_version) %>%
  tally() %>%
  print(n=50)
cross_walks %>%
  filter(nchar(from_code) == 4) %>%
  group_by(from_version, to_version) %>%
  tally() %>%
  print(n=50)
cross_walks %>%
  filter(nchar(from_code) == 5) %>%
  group_by(from_version, to_version) %>%
  tally() %>%
  print(n=50)

# create df with all NUTS codes that ever existed
all_nuts_codes <- cross_walks %>%
  select(code = from_code, version = from_version) %>%
  arrange(code, version) %>%
  distinct()

# all classifications should be 3 hierarchy levels times 5 classification years
all_nuts_codes %>%
  group_by(version) %>%
  tally()

# CHECK CODES AGAINST THE GISCO BOUNDARIES
#------------------------------------------
# GISCO publishes the attribute table of the NUTS boundaries of every version.
# For the countries covered by the matrices, the codes of each version should
# be identical to the codes in these tables. Requires an internet connection.
gisco_url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/csv/NUTS_AT_%s.csv"

gisco_diff <- all_nuts_codes %>%
  distinct(version) %>%
  pull(version) %>%
  map_dfr(function(v) {
    matrices <- all_nuts_codes %>% filter(version == v) %>% pull(code)
    gisco <- read_csv(sprintf(gisco_url, v), show_col_types = FALSE) %>%
      # levels 1 to 3 of the countries covered by the matrices
      filter(nchar(NUTS_ID) %in% 3:5,
             substr(NUTS_ID, 1, 2) %in% substr(matrices, 1, 2)) %>%
      pull(NUTS_ID)
    bind_rows(
      tibble(version = v, code = setdiff(matrices, gisco), only_in = "matrices"),
      tibble(version = v, code = setdiff(gisco, matrices), only_in = "gisco")
    )
  }) %>%
  arrange(version, code)

# Deviations in release 2025.08 that are known and not (yet) fixed:
# - ELZ, ELZZ, ELZZZ: extra-regio codes of Greece without GISCO geometry
#   (336 km2, probably Mount Athos), see ropensci/nuts#7
# - NO0B2: Svalbard is missing in the matrices
gisco_diff_known <- tribble(
  ~version, ~code, ~only_in,
  "2013", "ELZ", "matrices",
  "2013", "ELZZ", "matrices",
  "2013", "ELZZZ", "matrices",
  "2016", "ELZ", "matrices",
  "2016", "ELZZ", "matrices",
  "2016", "ELZZZ", "matrices",
  "2021", "NO0B2", "gisco",
  "2024", "NO0B2", "gisco"
)

# Known deviations
gisco_diff %>% print(n = 50)

# Any other deviation stops the script, e.g. Trøndelag and Northern Ireland
# without the patches above
gisco_diff_new <- anti_join(gisco_diff, gisco_diff_known,
                            by = c("version", "code", "only_in"))
if (nrow(gisco_diff_new) > 0) {
  print(gisco_diff_new, n = 100)
  stop("Codes differ from the GISCO boundaries, see above.")
}
# Known deviations that have disappeared should be removed from the list
stopifnot(nrow(anti_join(gisco_diff_known, gisco_diff,
                         by = c("version", "code", "only_in"))) == 0)

# We might want to add aggregate gains and losses of each region:
# changes <- cross_walks %>%
#   filter(from_code > to_code)
#
# take <- changes %>%
#   group_by(to_code) %>%
#   summarise(take_areaKm = sum(areaKm)) %>%
#   ungroup()
# give <- changes %>%
#   group_by(from_code) %>%
#   summarise(give_areaKm = sum(areaKm)) %>%
#   ungroup()
#
# # Add gives, takes and constant
# all_nuts_codes <- all_nuts_codes %>%
#   left_join(take, by = c("code" = "to_code")) %>%
#   left_join(give, by = c("code" = "from_code")) %>%
#   mutate_at(vars(take_areaKm, give_areaKm), list(~ifelse(is.na(.), 0, .)))


# ADDING COUNTRY STRINGS
#------------------------
# Remark: Greece changed its key from GR to EL!!!!!

# Situation at 7.10.2022
# url <- "https://publications.europa.eu/code/en/en-5000600.htm"
# countries <- read_html(url) %>%
#   html_nodes(xpath = '//*[@id="maincontent"]/div[1]/table') %>%
#   html_table() %>%
#   .[[1]] %>%
#   set_names(c("code_2d", "country"))
#
# countries <- bind_rows(
#   countries,
#   data.frame(code_2d = "GR", country = "Greece")
# )
# save(countries, file = "data-raw/countries.Rda")

load(file = "data-raw/countries.Rda")

# Append to all_nuts_codes
all_nuts_codes <- all_nuts_codes %>%
  mutate(code_2d = substr(code, 0, 2)) %>%
  left_join(countries) %>%
  select(-code_2d)

# Append to cross_walks
cross_walks <- cross_walks %>%
  mutate(code_2d = substr(from_code, 0, 2)) %>%
  left_join(countries) %>%
  select(-code_2d)

# Note Greece
cross_walks %>%
  mutate(code_2d = substr(from_code, 0, 2)) %>%
  distinct(country, code_2d) %>%
  arrange(country) %>%
  print(n=100)

cross_walks <- cross_walks %>%
  select(from_code, to_code, from_version, to_version,
         level, country, names(.))

# Dimensions
all_nuts_codes %>% dim()
all_nuts_codes %>% distinct(code, version) %>% dim()
cross_walks %>% dim()
cross_walks %>% distinct(from_code, to_code, from_version, to_version) %>% dim()

# Versions
all_nuts_codes %>% distinct(version)
cross_walks %>% distinct(from_version, to_version) %>% arrange(from_version, to_version)

# Save data separately https://r-pkgs.org/data.html#sec-data-data
usethis::use_data(cross_walks, overwrite = TRUE)
usethis::use_data(all_nuts_codes, overwrite = TRUE)
