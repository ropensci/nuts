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
#'
#' @return A tibble containing NUTS codes, aggregated variable values, and possibly grouping variables.
#'
#' @import crayon
#' @import dplyr
#' @import stringr
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
           multiple_versions = "break") {
    # CODE BREAKING CHECKS
    #------------------------
    # Input checks
    if (any(class(data) != "nuts.classified"))
      stop("Input 'data' must be a nuts.classified-object.")

    if (is.null(variables))
      stop("Input 'variables' cannot be NULL.")

    if (any(names(variables) %in% colnames(data[[1]])) == F)
      stop("Input 'variables' not found in the provided data frame.")

    if (any(!(unlist(variables) %in% c("absolute", "relative"))))
      stop("Variable type(s) not found. Use one of the following: 'absolute' or 'relative'.")

    if (!is.numeric(to_level))
      stop("Input 'to_level' invalid. Must be 1 or 2.")

    if (!to_level %in% c(1, 2))
      stop("Input 'to_level' invalid. Must be 1 or 2.")

    if (!is.null(weight)) {
      if (!weight %in% c("areaKm", "pop18", "pop11", "artif_surf18",
                         "artif_surf12"))
        stop(
          "Input 'weight' invalid. Must be 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
        )
    }

    if (!is.logical(missing_rm))
      stop("Input 'missing_rm' invalid. Must be TRUE/FALSE.")

    if (!multiple_versions %in% c("break", "most_frequent"))
      stop("Input 'multiple_versions' invalid. Must be 'break' or 'most_frequent'.")

    # Prepare data
    group_vars <- attributes(data)$groups
    data_versions <- data[[2]]
    data <- data[[1]]


    # Check whether user is trying to aggregate to a lower level than
    # current one
    if (data$from_level[1] < to_level) {
      stop(
        paste0(
          "Provided NUTS codes are on a higher level than input 'to_level'. Only data aggregation to a higher NUTS level allowed."
        )
      )
    }

    if (data$from_level[1] == to_level) {
      stop(paste0("NUTS codes already at level " , to_level , "."))
    }

    # Check if NUTS codes can be converted
    all_nuts_codes <- get("all_nuts_codes")
    check_nuts_codes <- data$from_code %in% all_nuts_codes$code
    if (length(data$from_code[check_nuts_codes]) == 0) {
      stop("\nNUTS codes are not recognized and cannot be converted.")
    }


    # CONVERSION POSSIBLE
    #----------------------
    # Welcome information
    cat(blue$bold("\nConverting level of NUTS codes"))
    cat(blue("\n------------------------------"))

    # CONVERSION BETWEEN DIFFERENT NUTS LEVELS
    cat(blue(
      paste0(
        "\n=> Aggregate from NUTS regional level ",
        red(data$from_level[1]),
        " to ",
        red(to_level),
        "."
      )
    ))

    # Check which NUTS codes can be converted
    if (length(data$from_code[check_nuts_codes]) == length(data$from_code)) {
      cat(blue("\n=> All NUTS codes can be converted."))

    } else if (length(data$from_code[check_nuts_codes]) < length(data$from_code) &
               length(data$from_code[check_nuts_codes]) > 0) {
      text <-
        paste0(
          "\n=> These NUTS codes cannot be converted and"  %+% red(" are dropped ") %+% "from the dataset: ",
          red(paste0(unique(
            data$from_code[!check_nuts_codes]
          ), collapse = ", ")),
          "."
        )
      cat(blue(text))

      data <- data[check_nuts_codes,]
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
    if (multi_versions_A > multi_versions_B) {
      cat(blue(
        paste0(
          "\n=> Within " %+% red("groups ") %+% "defined by ",
          red(paste0(group_vars, collapse = " x ")),
          ".",
          "\n==>"  %+% red(" Multiple ") %+% "NUTS code versions. "
        )
      ))

      if (multiple_versions == "break") {
        stop(
          "Mixed NUTS versions within groups! Please make sure the data contains only one version per group. Alternatively, keep only the codes belonging to the 'most_frequent' version using the argument 'multiple_versions'."        )

      } else if (multiple_versions == "most_frequent") {
        data_versions <- data_versions %>%
          group_by_at(vars(any_of(c(group_vars)))) %>%
          slice(1) %>%
          ungroup()

        data_multi_versions <-
          anti_join(data, data_versions, by = c("from_version", group_vars))
        data <-
          inner_join(data, data_versions, by = c("from_version", group_vars))

        cat(blue(
          paste0(
            "Choosing most frequent version within group and" %+% red(" dropping "),
            nrow(data_multi_versions),
            " row(s)."
          )
        ))
      }
      paste_grouping = F
    } else {
      paste_grouping = T
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

    if (paste_grouping) {
      cat(blue(paste0(
        "\n=> Within " %+% red("groups ") %+% "defined by ",
        red(paste0(group_vars, collapse = " x ")),
        "."
      )))

    }

    if (nrow(missing) > 0) {
      cat(
        blue(
          "\n==>" %+% red(" Missing ") %+% "NUTS codes in data. No values are calculated for regions associated with missing NUTS codes. Ensure that the input data is complete."
        )
      )
    } else if (nrow(missing) == 0) {
      cat(blue("\n==> No missing NUTS codes."))
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
      summarise_at(vars(all_of(abs_vars)), list( ~ sum(., na.rm = missing_rm))) %>%
      ungroup()

    # - Weigh relative values according to regional size
    rel_data <- data %>%
      select(-all_of(abs_vars)) %>%
      group_by_at(vars(any_of(c(
        "to_code", group_vars
      )))) %>%
      summarise_at(vars(all_of(rel_vars)),
                   list( ~ sum(. * .data$w, na.rm = missing_rm) / sum(.data$w))) %>%
      ungroup()

    data <- abs_data %>%
      full_join(rel_data, by = c("to_code", group_vars))
    # - Done

    cat("\n")

    return(data)
  }
