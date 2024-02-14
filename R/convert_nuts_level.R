#' Convert between NUTS levels
#'
#' `convert_nuts_level()` transforms regional NUTS data between NUTS levels
#'
#' @param data A nuts.classified object returned by [`classify_nuts()`].
#' @param to_level Number corresponding to desired NUTS level: `1`, `2` or `3`.
#' @param variables Named character specifying variable names and variable type (`'absolute'` or `'relative'`), e.g. `c('var_name' = 'absolute')`.
#' @param weight String with name of the weight used for conversion. Can be area size `'areaKm'` (default),
#' population in 2011 `'pop11'` or 2018 `'pop18'`, or artificial surfaces in 2012 `'artif_surf12'` and 2018 `'artif_surf18'`.
#' @param missing_rm Boolean that is FALSE by default. TRUE removes regional flows that depart from missing NUTS codes.
#' @param multiple_versions By default equal to `'break'`, throwing an error when providing multiple NUTS versions within groups.
#' If set to `'most_frequent'` data is converted using the best-matching NUTS version.
#' @param quiet Suppress messages and warnings. `FALSE` by default.
#'
#' @return A tibble containing NUTS codes, aggregated variable values, and possibly grouping variables.
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
#'# Aggregate from NUTS 3 to 2 by indicator x year
#' manure %>%
#'   filter(nchar(geo) == 5) %>%
#'   classify_nuts(nuts_code = "geo",
#'                 group_vars = c('indic_ag','time')) %>%
#'   # Group vars are automatically passed on
#'   convert_nuts_level(to_level = 2,
#'                      variables = c('values'= 'absolute'),
#'                      weight = 'pop18')
#'
#' @export

convert_nuts_level <-
  function(data = data,
           to_level = to_level,
           variables = variables,
           weight = NULL,
           missing_rm = FALSE,
           multiple_versions = "break",
           quiet=FALSE) {

    if (quiet) {
      old <- options(cli.default_handler = function(...) { })
      on.exit(options(old), add = TRUE)
    }

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
    if (any(class(data) != "nuts.classified"))
      cli_abort(
        "Input {.arg data} must be a nuts.classified-object, not {.obj_type_friendly {data}}."
      )

    if (is.null(variables))
      cli_abort("Input {.arg variables} cannot be NULL.")

    if (any(names(variables) %in% colnames(data[["data"]])) == F)
      cli_abort("Input {.arg variables} not found in the provided data frame.")

    if (any(!(unlist(variables) %in% c("absolute", "relative"))))
      cli_abort("Variable type(s) not found. Use one of the following: 'absolute' or 'relative'.")

    if (!is.numeric(to_level) | !to_level %in% c(1, 2))
      cli_abort("Input {.arg to_level} invalid. Must be 1 or 2.")

    if (!is.null(weight)) {
      if (!weight %in% c("areaKm", "pop18", "pop11", "artif_surf18",
                         "artif_surf12"))
        cli_abort(
          "Input {.arg weight} invalid. Must be 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
        )
    }

    if (!is.logical(missing_rm))
      cli_abort(
        "Input {.arg missing_rm} invalid. Must be TRUE/FALSE, not {.obj_type_friendly {missing_rm}}."
      )

    if (!multiple_versions %in% c("break", "most_frequent"))
      cli_abort("Input {.arg multiple_versions} invalid. Must be 'break' or 'most_frequent'.")

    # Prepare data
    group_vars <- attributes(data)$groups
    data_versions <- data[["versions_data"]]
    data <- data[["data"]]


    # Check whether user is trying to aggregate to a lower level than
    # current one
    if (data$from_level[1] < to_level)
      cli_abort(
        "Provided NUTS codes are on a higher level than input {.arg to_level}. Only data aggregation to a higher NUTS level allowed."
      )

    if (data$from_level[1] == to_level)
      cli_abort("NUTS codes already at level {to_level}.")

    # Check if NUTS codes can be converted
    all_nuts_codes <- get("all_nuts_codes")
    check_nuts_codes <- data$from_code %in% all_nuts_codes$code
    nr_nuts_codes_recognized <-
      length(data$from_code[check_nuts_codes])
    if (nr_nuts_codes_recognized == 0)
      cli_abort("NUTS codes are not recognized and cannot be converted.")

    # CONVERSION POSSIBLE
    #----------------------
    # CONVERSION BETWEEN DIFFERENT NUTS LEVELS
    aggregate_from_level <- data$from_level[1]
    message_conversion_levels <- c("i" = "{.blue Aggregate from NUTS regional level {.red {aggregate_from_level}} to {.red {to_level}}}.")

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

    # Test for multiple versions within groups
    multi_versions_A <- data %>%
      select(all_of(c(group_vars, "from_version"))) %>%
      distinct() %>%
      nrow()

    multi_versions_B <- data %>%
      select(all_of(c(group_vars))) %>%
      distinct() %>%
      nrow()

    # Use data_versions which is sorted for most frequent version within group
    if (multi_versions_A > multi_versions_B && multiple_versions == "break") {

      cli_abort(
        c(
          "Mixed NUTS versions within groups!"
          ,
          "Please make sure the data contains only one version per group. Alternatively, keep only the codes belonging to the 'most_frequent' version using the argument 'multiple_versions'."
        )
      )

    } else if (multi_versions_A > multi_versions_B && multiple_versions == "most_frequent") {
      data_versions <- data_versions %>%
        group_by_at(vars(any_of(c(group_vars)))) %>%
        slice(1) %>%
        ungroup()

      data_multi_versions <-
        anti_join(data, data_versions, by = c("from_version", group_vars))
      data <-
        inner_join(data, data_versions, by = c("from_version", group_vars))

      n_rows_dropped <- nrow(data_multi_versions)
      message_multiple_versions <- c("!" =  "{.blue Choosing most frequent version within group and {.red dropping} {n_rows_dropped} row{?s}.}")
    } else {
      message_multiple_versions <- c("v" =  "{.blue Version is {.red unique}.}")
    }
    # - Done

    # Prepare join with regional indicator stocks such that missing NUTS codes within groups are kept
    # - Create group structure
    group_structure <- data %>%
      select(any_of(c("from_level", "country", group_vars))) %>%
      distinct() %>%
      arrange(.data$from_level)

    # - Prepare stocks from cross_walks for subsetting and matching
    cross_walks <- get("cross_walks")
    stocks <- cross_walks %>%
      filter(.data$from_version == .data$to_version) %>%
      select(-c("from_version", "to_version", "to_code")) %>%
      distinct(.data$from_code, .keep_all = T)

    # - Expand all NUTS codes by all group combinations
    if (!is.null(group_vars)) {
      groups <- data %>%
        select(all_of(group_vars)) %>%
        select(-c("country")) %>%
        distinct()
      stocks_groups <- stocks %>% cross_join(groups)
    }


    # - Subset cross walks to desired countries, levels and versions
    stocks_groups <- stocks_groups %>%
      rename(from_level = .data$level) %>%
      inner_join(group_structure, by = c("from_level", group_vars))

    # - Add crosswalk keeping missing NUTS codes within group
    data <- data %>%
      right_join(stocks_groups, by = c("from_code", "from_level", group_vars)) %>%
      arrange(.data$from_code)
    rm(stocks, stocks_groups)

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

    # Get weight (default: area size)
    if (is.null(weight)) {
      data <- data %>%
        mutate(w = .data$areaKm)
    } else {
      data <- data %>%
        mutate(w = !!sym(weight))
    }

    # - Shorten NUTS code to desired level
    data <- data %>%
      mutate(to_code = substr(.data$from_code, 0, 2 + to_level))

    abs_vars <- names(variables[variables == "absolute"])
    rel_vars <- names(variables[variables == "relative"])

    # - Aggregate absolute values
    abs_data <- data %>%
      select(-all_of(rel_vars)) %>%
      group_by_at(vars(any_of(c(
        "to_code", group_vars
      )))) %>%
      summarise_at(vars(all_of(abs_vars)), list(~ sum(., na.rm = missing_rm))) %>%
      ungroup()

    # - Weigh relative values according to regional size
    rel_data <- data %>%
      select(-all_of(abs_vars)) %>%
      group_by_at(vars(any_of(c(
        "to_code", group_vars
      )))) %>%
      summarise_at(vars(all_of(rel_vars)),
                   list(~ sum(. * .data$w, na.rm = missing_rm) / sum(.data$w))) %>%
      ungroup()

    data <- abs_data %>%
      full_join(rel_data, by = c("to_code", group_vars))
    # - Done

    # Console Message
    #-----------------
    is_verbose_mode <- (getOption("nuts.verbose", "quiet") == "verbose")
    if (is_verbose_mode) {
      cli_h1("Converting version of NUTS codes")
      cli_bullets(
        c(
          "{.blue Within {.red groups} defined by {.red {group_vars}}:}",
          message_conversion_levels,
          message_can_be_converted,
          message_multiple_versions,
          message_missing_codes
        )
      )
    }

    return(data)
  }
