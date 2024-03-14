#' Convert between NUTS versions
#'
#' `nuts_convert_version()` transforms regional NUTS data between NUTS versions.
#'
#' @param data A nuts.classified object returned by [`nuts_classify()`].
#' @param to_version String with desired NUTS version the function should convert to. Possible versions: `'2006'`, `'2010'`, `'2013'`, `'2016'` or `'2021'`
#' @param variables Named character specifying variable names and variable type (`'absolute'` or `'relative'`) e.g. `c('var_name' = 'absolute')`
#' @param weight String with name of the weight used for conversion. Can be area size `'areaKm'` (default),
#' population in 2011 `'pop11'` or 2018 `'pop18'`, or artificial surfaces in 2012 `'artif_surf12'` and 2018 `'artif_surf18'`.
#' @param missing_rm Boolean that is FALSE by default. TRUE removes regional flows that depart from missing NUTS codes.
#' @param missing_weights_pct Boolean that is FALSE by default. TRUE computes the percentage of missing weights due to missing departing NUTS regions for each variable.
#' @param multiple_versions By default equal to `'error'`, when providing multiple NUTS versions within groups.
#' If set to `'most_frequent'` data is converted using the best-matching NUTS version.
#'
#' @return A tibble containing NUTS codes, converted variable values, and possibly grouping variables.
#'
#' @details Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and
#' `nuts.verbose = "verbose"` to switch messages back on.
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
#' # Convert NUTS 2 codes in Germany from 2006 to 2021 version
#' manure %>%
#'   filter(nchar(geo) == 4) %>%
#'   filter(indic_ag == 'I07A_EQ_Y') %>%
#'   filter(grepl('^DE', geo)) %>%
#'   filter(time == 2003) %>%
#'   select(-indic_ag, -time) %>%
#'   # Data now only varies at the NUTS code level
#'   nuts_classify(nuts_code = "geo") %>%
#'   nuts_convert_version(to_version = '2021',
#'                        weight = 'pop18',
#'                        variables = c('values' = 'absolute'))
#'
#'
#' # Convert NUTS 3 codes by country x year, classifying version first
#' manure %>%
#'   filter(nchar(geo) == 5) %>%
#'   filter(indic_ag == 'I07A_EQ_Y') %>%
#'   select(-indic_ag) %>%
#'   # Data now varies at the year x NUTS code level
#'   nuts_classify(nuts_code = 'geo', group_vars = c('time')) %>%
#'   nuts_convert_version(to_version = '2021',
#'                        weight = 'pop18',
#'                        variables = c('values' = 'absolute'))
#'
#'
#' @export
nuts_convert_version <-
  function(data,
           to_version,
           variables,
           weight = NULL,
           missing_rm = FALSE,
           missing_weights_pct = FALSE,
           multiple_versions = c("error", "most_frequent")) {

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
    if (!(inherits(data, "nuts.classified")))
      cli_abort(
        "Input {.arg data} must be a nuts.classified-object, not {.obj_type_friendly {data}}."
      )

    if (is.null(variables))
      cli_abort("Input {.arg variables} cannot be NULL.")

    if (any(!(names(variables) %in% colnames(data[["data"]]))))
      cli_abort("Input {.arg variables} not found in the provided data frame.")

    if (any(!(unlist(variables) %in% c("absolute", "relative"))))
      cli_abort("Variable type(s) not found. Use one of the following: 'absolute' or 'relative'.")

    if ((!any(to_version %in% c("2006", "2010", "2013", "2016", "2021"))) | length(to_version) > 1)
      cli_abort(
        "Input {.arg to_version} invalid. Make sure it is a string and only one of the version years 2006, 2010, 2013, 2016 or 2021."
      )

    if (!is.null(weight)) {
      if ((!any(weight %in% c("areaKm", "pop18", "pop11", "artif_surf18",
                          "artif_surf12"))) | length(weight) >  1)
        cli_abort(
          "Input {.arg weight} invalid. Must be either 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
        )
    }


    if (!is.logical(missing_rm))
      cli_abort(
        "Input {.arg missing_rm} invalid. Must be TRUE/FALSE, not {.obj_type_friendly {missing_rm}}."
      )

    if (!multiple_versions[1] %in% c("error", "most_frequent"))
      cli_abort("Input {.arg multiple_versions} invalid. Must be 'error' or 'most_frequent'.")

    # Prepare data
    group_vars <- attributes(data)$groups
    data_versions <- data[["versions_data"]]
    data <- data[["data"]]

    # Check if NUTS codes can be converted
    all_nuts_codes <- get("all_nuts_codes")
    check_nuts_codes <- data$from_code %in% all_nuts_codes$code
    nr_nuts_codes_recognized <-
      length(data$from_code[check_nuts_codes])
    if (nr_nuts_codes_recognized == 0)
      cli_abort("NUTS codes are not recognized and cannot be converted.")

    # CONVERSION POSSIBLE
    #----------------------
    # CONVERSION BETWEEN DIFFERENT NUTS VERSIONS
    versions_str = unique(data$from_version[!is.na(data$from_version)])
    versions_n = length(versions_str)
    message_conversion_versions <- c("i" = "{.blue Converting NUTS codes in {versions_n} version{?s} {.red {versions_str}} to version {.red {to_version}}.}")

    # Check which NUTS codes can be converted
    nr_nuts_codes_recognized <-
      length(data$from_code[check_nuts_codes])
    nr_nuts_codes <- length(data$from_code)
    dropped_codes <- unique(data$from_code[!check_nuts_codes])
    if (nr_nuts_codes_recognized == nr_nuts_codes) {
      message_can_be_converted <- c("v" = "{.blue All NUTS codes can be converted.}")
    } else if (nr_nuts_codes_recognized < nr_nuts_codes &&
               nr_nuts_codes_recognized > 0) {
      message_can_be_converted <- c("x" = "{.blue These NUTS codes cannot be converted and {.red are dropped}: {.red {dropped_codes}}.}")
      data <- data[check_nuts_codes, ]
    }

    # Test for multiple versions
    data <- nuts_test_multiple_versions(group_vars = group_vars, multiple_versions = multiple_versions,
                                        data_versions = data_versions, data = data)
    n_rows_dropped <- data[["n_rows_dropped"]]
    message_multiple_versions <- data[["message_multiple_versions"]]
    data <- data[["data"]]

    # Prepare join with cross walk such that missing NUTS codes within groups are kept
    # - Filter cross walks to desired version
    cross_walks <- get("cross_walks")[cross_walks$to_version == to_version, ]

    # - Create group structure
    group_structure <- data %>%
      select(all_of(unique(c("from_version", "from_level", "country", group_vars)))) %>%
      distinct() %>%
      arrange(.data$from_level)

    # - Expand all NUTS codes by all group combinations
    if (!is.null(group_vars)) {
      groups <- data %>%
        select(all_of(group_vars)) %>%
        select(-c("country")) %>%
        distinct()
      cross_walks_groups <- cross_walks %>% cross_join(groups)
    } else {
      cross_walks_groups <- cross_walks
    }

    # - Subset cross walks to desired countries, levels and from_versions
    cross_walks_groups <- cross_walks_groups %>%
      rename(from_level = .data$level) %>%
      inner_join(group_structure,
                 by = c("from_version", "from_level", group_vars))

    # - Add crosswalk keeping missing departing codes within groups
    data <- data %>%
      right_join(cross_walks_groups,
                 by = c("from_code", "from_version", group_vars)) %>%
      arrange(.data$from_code)
    rm(cross_walks_groups, cross_walks)

    # - Missing NUTS codes
    missing <- data %>%
      select(all_of(names(variables))) %>%
      filter(if_any(names(variables), ~ is.na(.)))

    # - Alert missing
    if (nrow(missing) > 0) {
      message_missing_codes <- c("x" = "{.blue {.red Missing} NUTS codes in data. No values are calculated for regions associated with missing NUTS codes. Ensure that the input data is complete.}")

      } else if (nrow(missing) == 0) {
      message_missing_codes <- c("v" = "{.blue No {.red missing} NUTS codes.}")
    }
    rm(missing)

    # - Get weight (default: area size)
    if (is.null(weight)) {
      data <- data %>%
        mutate(w = .data$areaKm)
    } else {
      data <- data %>%
        mutate(w = !!sym(weight))
    }

    # - Identify variables with absolute and relative values
    abs_vars <- names(variables[variables == "absolute"])
    rel_vars <- names(variables[variables == "relative"])

    # - Convert absolute values
    abs_data <- data %>%
      select(-all_of(rel_vars)) %>%
      group_by(pick(c(
        "from_code", group_vars
      ))) %>%
      mutate(w_sh = .data$w / sum(.data$w)) %>%
      ungroup() %>%
      select(-.data$w) %>%
      group_by(pick(c(
        "to_code", group_vars
      ))) %>%
      summarise(across(abs_vars, ~sum(.x * .data$w_sh, na.rm = missing_rm))) %>%
      ungroup() %>%
      # - Add version
      mutate(to_version = to_version) %>%
      relocate(c("to_code", "to_version"))

    # - Convert relative values
    rel_data <- data %>%
      select(-all_of(abs_vars)) %>%
      group_by(pick(c(
        "to_code", group_vars
      ))) %>%
      summarise(across(rel_vars, ~{sum(.x * .data$w, na.rm = missing_rm) /
                                                   sum(.data$w)})) %>%
      ungroup() %>%
      # - Add version
      mutate(to_version = to_version) %>%
      relocate(c("to_code", "to_version"))


    # - Compute share of missing weights for each variable
    if(missing_weights_pct){
      # - For absolute variables
      abs_missing_weights <- data %>%
        select(-all_of(rel_vars)) %>%
        group_by(pick(c(
          "from_code", group_vars
        ))) %>%
        mutate(w_sh = .data$w / sum(.data$w)) %>%
        ungroup() %>%
        select(-.data$w) %>%
        # - Create weight for every variable that is missing when variable is missing
        mutate(across(abs_vars, ~ifelse(is.na(.x), NA_real_,  .data$w_sh), .names = "{col}_na_w")) %>%
        group_by(pick(c(
          "to_code", group_vars
        ))) %>%
        # - Summarize across weights
        summarise(across(matches("w"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        # Compute share of missing weights
        mutate(across(matches("_na_w"), ~ 100 - .x / w_sh * 100)) %>%
        select(-.data$w_sh)

      # - For relative variables
      rel_missing_weights <- data %>%
        select(-all_of(abs_vars)) %>%
        mutate(across(rel_vars, ~ifelse(is.na(.x), NA_real_,  .data$w), .names = "{col}_na_w")) %>%
        group_by(pick(c(
          "to_code", group_vars
        ))) %>%
        summarise(across(matches("w"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        # Compute share of missing weights
        mutate(across(matches("_na_w"), ~ 100 - .x / w * 100)) %>%
        select(-.data$w)

    }

    # Overwrite data with absolute and relative values at to_code x group level
    data <- abs_data %>%
      full_join(rel_data, by = c("to_code", "to_version", group_vars))
    rm(abs_data, rel_data)

    # Add missing weights if requested
    if(missing_weights_pct){
      data <- data %>%
        left_join(abs_missing_weights, by = c("to_code", group_vars)) %>%
        left_join(rel_missing_weights, by = c("to_code", group_vars))
      rm(abs_missing_weights, rel_missing_weights)
    }
    # - done

    # Console Message
    #-----------------
    is_verbose_mode <- (getOption("nuts.verbose", "verbose") == "verbose")
    if (is_verbose_mode) {
      cli_h1("Converting version of NUTS codes")
      cli_bullets(
        c(
          "{.blue Within {.red groups} defined by {.red {group_vars}}:}",
          message_conversion_versions,
          message_can_be_converted,
          message_multiple_versions,
          message_missing_codes
        )
      )
    }

    return(as_tibble(data))
  }
