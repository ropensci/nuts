#' Helper function to test for multiple versions
#'
#' `nuts_test_multiple_versions` is called from either `nuts_convert_version` or `nuts_aggregate`
#' to selects the most frequent version within groups or throw an error.
#'
#' @return A tibble containing NUTS codes and a message with the results of the test.
#'
#' @export

nuts_test_multiple_versions = function(group_vars, multiple_versions, data_versions, data) {

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
    message_multiple_versions <-
      c("v" =  "{.blue Version is {.red unique}.}")
  }

  data_list <- list(data, message_multiple_versions)
  names(data_list) <- c("data", "message_multiple_versions")

  return(data_list)
}
