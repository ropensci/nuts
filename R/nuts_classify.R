#' Classify version of NUTS codes
#'
#' `nuts_classify()` can identify the NUTS version year and level from a variable containing NUTS codes.
#'
#' @param data A data frame or tibble that contains a variable with NUTS `1`, `2` or `3` codes and possibly other variables.
#' NUTS codes must be of the same level and need to be unique, unless additional grouping variables are specified. No
#' duplicate NUTS codes within groups allowed.
#' @param nuts_code Variable name containing NUTS codes
#' @param group_vars Variable name(s) for classification within groups. `nuts_classify()` always computes overlap within country. Hence, country variables should not be specified. `NULL` by default.
#' @param ties Picks `'most_recent'` or `'oldest'` version when overlap is identical across multiple NUTS versions. `'most_recent'`
#' by default.
#'
#' @return A list of three tibbles. The first tibble contains the original data with the classified NUTS version, level, and country.
#' The second tibble lists the group-specific overlap with each NUTS version. The third tibble shows missing NUTS codes
#' for each group.
#'
#' The output can be passed to [nuts_convert_version()] to convert data across NUTS versions and [nuts_aggregate()] to aggregate across NUTS levels.
#'
#' @details Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and
#' `nuts.verbose = "verbose"` to switch messages back on.
#'
#'
#' @examples
#' library(dplyr)
#'
#' # Load EUROSTAT data of manure storage deposits
#' data(manure)
#'
#' # Data varies at the NUTS level x indicator x year x country x NUTS code level
#' head(manure)
#'
#'# Classify version of NUTS 2 codes in Germany
#'manure %>%
#'  filter(nchar(geo) == 4) %>%
#'  filter(indic_ag == 'I07A_EQ_Y') %>%
#'  filter(grepl('^DE', geo)) %>%
#'  filter(time == 2003) %>%
#'  select(-indic_ag, -time) %>%
#'  # Data varies at the NUTS code level
#'  nuts_classify(nuts_code = 'geo')
#'
#'# Classify version of NUTS 3 codes within country and year
#' manure %>%
#'   filter(nchar(geo) == 5) %>%
#'   filter(indic_ag == 'I07A_EQ_Y') %>%
#'   select(-indic_ag) %>%
#'   # Data varies at the year x country x NUTS code level. The country grouping
#'   # is always used by default.
#'   nuts_classify(nuts_code = 'geo', group_vars = 'time')
#'
#'
#' @export
nuts_classify <- function(data,
                          nuts_code,
                          group_vars = NULL,
                          ties = c("most_recent", "oldest")) {

    # DEFINE CLI DIVs
    #-----------------------
    cli_div(theme = list(
      span.red = list(color = "red"),
      span.blue = list(color = "blue")
      )
      )

    # CODE BREAKING CHECKS
    #------------------------
    # Input checks
    if (!is.data.frame(data))
      cli_abort("Input {.arg data} must be a data frame or tibble, not {.obj_type_friendly {data}}.")
    if (length(nuts_code) > 1)
      cli_abort("Input {.arg nuts_code} can only be a single string.")
    if (!is.character(nuts_code))
      cli_abort("Input {.arg nuts_code} must be provided as a string, not {.obj_type_friendly {nuts_code}}.")
    if (!(nuts_code %in% colnames(data)))
        cli_abort("Input {.arg nuts_code} not found in the provided data frame.")
    if (!ties[1] %in% c("most_recent", "oldest"))
      cli_abort("Input {.arg ties} must be 'most_recent' or 'oldest'.")
    if ("version" %in% names(data))
      cli::cli_abort("Please rename the variable {.arg version} in the provided data frame to avoid conflicts. The function {.fn classify_nuts} generates a variable with the same name.")

    # Prepare data set
    data <- data %>%
      as_tibble() %>%
      rename(from_code = !!sym(nuts_code))

    # Simple NUTS code check
    if (any(!(grepl("^[A-Za-z]{2}[A-Za-z0-9]{1,3}$", data$from_code)))){
      invalid_codes  <- unique(data$from_code[!grepl("^[A-Za-z]{2}[A-Za-z0-9]{1,3}$", data$from_code)])
      cli_abort(
        c(
          "Variable {.var {nuts_code}} contains invalid NUTS codes.",
          "NUTS codes must start with two letters, followed by one (level 1) to three (level 3) alphanumeric characters, all uppercase.",
          "Invalid codes: {invalid_codes}"
          )
        )
    }

    # Grouping vars, country identified by default
    if (!is.null(group_vars)) {
      if (any(!(group_vars %in% colnames(data)))) {
        cli_abort("Input {.arg group_vars} not found in the provided data frame.")
      }
      group_vars <- c("country", group_vars)
    } else {
      group_vars <- "country"
    }


    # Test if NUTS codes are uniquely identified within groups
    distinct_N <- data %>%
      select(any_of(c("from_code", group_vars))) %>%
      distinct() %>%
      nrow()

    if (distinct_N < nrow(data)) {
      duplicate_codes <- data %>%
        group_by(pick(c(
          "from_code", group_vars[group_vars != "country"]
        ))) %>%
        filter(n() > 1) %>%
        distinct(.data$from_code) %>%
        pull()
      cli_abort(
        c( "Duplicate NUTS codes found.",
           "=> Please only use unique NUTS codes or specify a grouping variable in {.arg group_vars}.",
           "Duplicate codes: {duplicate_codes}."
           )
      )
    }

    # Test if NUTS codes of different levels present
    nr_different_nuts_levels = length(unique(nchar(data$from_code)))
    if (nr_different_nuts_levels != 1) {
      cli_abort(
        "Data contains NUTS codes from multiple levels ({sort(unique(nchar(data$from_code) - 2))}).",
        "=> Please classify different levels separately."
      )
    }

    # CLASSIFICATION POSSIBLE
    #-------------------------
    # Check for NUTS codes that cannot be classified
    all_nuts_codes <- get("all_nuts_codes")
    codes_not_found <-
      data$from_code[!data$from_code %in% all_nuts_codes$code]
    if (length(codes_not_found) > 0) {
      message_codes_not_found <- c("!" = "{.blue These NUTS codes cannot be identified or classified: {.red {codes_not_found}}.}")
    } else (
      message_codes_not_found <- c("v" = "{.blue All NUTS codes can be identified and classified.}")
    )

    # A. CLASSIFY LEVEL
    #-----------------------
    data <- data %>%
      mutate(from_level = case_when(
        nchar(from_code) == 3 ~ 1,
        nchar(from_code) == 4 ~ 2,
        nchar(from_code) == 5 ~ 3,
        .default = NA
      ))
    # -done


    # B. CLASSIFY VERSION
    #-----------------------
    # Assess within group overlap of each version
    # - Relationship can be many-to-many
    data <- data %>%
      left_join(all_nuts_codes, by = join_by("from_code" == "code")) %>%
      rename(from_version = .data$version)

    # - Exclude codes that cannot be classified
    data_na <- data %>%
      filter(is.na(.data$from_version))

    # - Compute overlap for each version within groups
    data <- data %>%
      filter(!is.na(.data$from_version)) %>%
      group_by(pick(group_vars)) %>%
      mutate(from_code_N_dis = n_distinct(.data$from_code)) %>%
      ungroup() %>%
      group_by(pick(c(
        "from_version", group_vars))) %>%
      mutate(from_version_N = n()) %>%
      ungroup() %>%
      mutate(overlap_perc = round(.data$from_version_N / .data$from_code_N_dis * 100, 2)) %>%
      select(-c("from_code_N_dis", "from_version_N")) %>%
      # - Add them again with version = NA
      bind_rows(data_na)
    rm(data_na)

    # - Sort for best version in case of ties
    if (ties[1] == "oldest") {
      data <- data %>%
        arrange(desc(.data$overlap_perc), .data$from_version)
    } else {
      # - Sort for most recent version is default
      data <- data %>%
        arrange(desc(.data$overlap_perc), desc(.data$from_version))
    }

    # - Save overlap report by group for all versions, keeping grouping
    data_all_versions <- data %>%
      select(all_of(c(
        "from_version", group_vars, "overlap_perc"
      ))) %>%
      distinct() %>%
      arrange(pick(group_vars)) %>%
      group_by(pick(group_vars))

    # - Chose best version
    data <- data %>%
      group_by(pick(c(
        "from_code", group_vars))) %>%
      slice(1) %>%
      ungroup()

    # - Check if there is variation within groups
    pct_overlap_within_groups <- unique(data$overlap_perc[!is.na(data$from_version)])
    if (any(pct_overlap_within_groups < 100)) {
      message_multiple_versions <- c("x" = "{.blue {.red Multiple} NUTS versions classified. See the tibble 'versions_data' in the output.}")
    } else {
      message_multiple_versions <- c("v" = "{.blue {.red Unique} NUTS version classified.}")
    }

    # - Clean data
    data <- data %>%
      select(c(
        "from_code",
        "from_version",
        "from_level",
        "country",
        names(data)
      )) %>%
      select(-c("overlap_perc"))

    # Prepare tibble with missing NUTS codes at national level for each group
    # - Create group structure
    group_structure <- data %>%
      select(all_of(c(
        "from_version", "from_level", group_vars
      ))) %>%
      distinct() %>%
      arrange(.data$from_level)

    # - Prepare tibble with all NUTS codes
    all_nuts_codes <- all_nuts_codes %>%
      mutate(from_level = case_when(
        nchar(.data$code) == 3 ~ 1,
        nchar(.data$code) == 4 ~ 2,
        nchar(.data$code) == 5 ~ 3,
        .default = NA
      ))

    # - Expand all NUTS codes by all group combinations
    if (!is.null(group_vars)) {
      groups <- data %>%
        select(all_of(group_vars)) %>%
        select(-"country") %>%
        distinct()
      all_nuts_codes <- all_nuts_codes %>% cross_join(groups)
    }

    # - Subset all NUTS codes by existing group structures
    # - Save difference between all NUTS codes and codes in data by group
    data_missing_nuts <- all_nuts_codes %>%
      rename(from_code = .data$code,
             from_version = .data$version) %>%
      inner_join(group_structure,
                 by = c("from_version", "from_level" ,  group_vars)) %>%
      anti_join(data,
                by = c("from_code", "from_version", "from_level", group_vars)) %>%
      select(c("from_code", "from_version", "from_level", group_vars)) %>%
      group_by(pick(c(
        "from_version", group_vars
      )))


    # - Alert missing
    if (nrow(data_missing_nuts) == 0) {
      message_missing_codes <- c("v" =  "{.blue No missing NUTS codes.}")
    } else if (nrow(data_missing_nuts) > 0) {
      message_missing_codes <- c( "x" = "{.blue {.red Missing} NUTS codes detected. See the tibble 'missing_data' in the output.}")
    }
    # - done

    # Console Message
    #-----------------
    is_verbose_mode <-
      (getOption("nuts.verbose", "verbose") == "verbose")
    if (is_verbose_mode) {
      cli_h1("Classifying version of NUTS codes")
      cli_bullets(
        c(
          "{.blue Within {.red groups} defined by {.red {group_vars}}:}",
          message_codes_not_found,
          message_multiple_versions,
          message_missing_codes
        )
      )
    }

    # OUTPUT
    #--------
    output <- list(
      data = data,
      versions_data = data_all_versions %>% ungroup( ),
      missing_data = data_missing_nuts %>% ungroup( )
    )

    attr(output, "groups") <- group_vars
    class(output) <- c("nuts.classified", class(output))

    return(output)
  }
