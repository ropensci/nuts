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

cross_walks <- list()
for( i in seq_along(csv_files) ){

  #i = 1

  # read conversion matrix supplied by the JRC
  t <- read_csv( csv_files[i] )
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
