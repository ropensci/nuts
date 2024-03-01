#' Helper function to test for multiple versions
#'
#' `nuts_test_multiple_versions` is called from either `nuts_convert_version` or `nuts_aggregate`
#' to selects the most frequent version within groups or throw an error.
#'
#' @param group_vars Variable name(s) for classification within groups. Always computes overlap within country. `NULL` by default.
#' @param multiple_versions By default equal to `'error'`, when providing multiple NUTS versions within groups.
#' @param data_versions Data versions
#' @param data A nuts.classified object returned by [`nuts_classify()`].
#'
#' @return A tibble containing NUTS codes, the potential number of rows dropped and a message with the results of the test.
#'
#' @examples
#' library(dplyr)
#' df <- manure %>%
#'   filter(nchar(geo) == 5) %>%
#'   select(geo, indic_ag, values) %>%
#'   distinct(geo,  .keep_all = TRUE) %>%
#'   nuts_classify(nuts_code = "geo",
#'                 group_vars = "indic_ag",
#'                 data = .)
#'
#' nuts_test_multiple_versions(group_vars = "indic_ag",
#'                             multiple_versions = "most_frequent",
#'                             data_versions = df$versions_data,
#'                             data = df$data)
#'
#' @export

nuts_test_multiple_versions = function(group_vars,
                                       multiple_versions,
                                       data_versions,
                                       data) {
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
  if (multi_versions_A > multi_versions_B &&
      multiple_versions[1] == "error") {
    cli_abort(
      c(
        "Mixed NUTS versions within groups!"
        ,
        "Please make sure the data contains only one version per group. Alternatively, keep only the codes belonging to the 'most_frequent' version using the argument 'multiple_versions'."
      )
    )

  } else if (multi_versions_A > multi_versions_B &&
             multiple_versions[1] == "most_frequent") {
    data_versions <- data_versions %>%
      group_by_at(vars(any_of(c(group_vars)))) %>%
      slice(1) %>%
      ungroup()

    data_multi_versions <-
      anti_join(data, data_versions, by = c("from_version", group_vars))
    data <-
      inner_join(data, data_versions, by = c("from_version", group_vars))

    n_rows_dropped <- nrow(data_multi_versions)
    message_multiple_versions <-
      c("!" =  "{.blue Choosing most frequent version within group and {.red dropping} {n_rows_dropped} row{?s}.}")
  } else {
    n_rows_dropped <- 0
    message_multiple_versions <-
      c("v" =  "{.blue Version is {.red unique}.}")
  }

  data_list <- list(data, n_rows_dropped, message_multiple_versions)
  names(data_list) <-
    c("data", "n_rows_dropped", "message_multiple_versions")

  return(data_list)
}
