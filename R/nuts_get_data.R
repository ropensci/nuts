#' Return classified NUTS data
#'
#' `nuts_get_data()` returns the classified data after running `nuts_classify()`.
#'
#' @param data A nuts.classified object returned by [`nuts_classify()`].
#'
#' @details Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and
#' `nuts.verbose = "verbose"` to switch messages back on.
#'
#' @return A tibble containing the original data with the classified NUTS version, level, and country.
#'
#' @examples
#' library(dplyr)
#'
#' # Load EUROSTAT data of manure storage deposits
#' data(manure)
#'
#'# Classify version of NUTS 2 codes in Germany
#' classified <- manure %>%
#'    filter(nchar(geo) == 4) %>%
#'    filter(indic_ag == 'I07A_EQ_Y') %>%
#'    filter(grepl('^DE', geo)) %>%
#'    filter(time == 2003) %>%
#'    select(-indic_ag, -time) %>%
#'    # Data varies at the NUTS code level
#'    nuts_classify(nuts_code = 'geo')
#'
#' nuts_get_data(classified)
#'
#' @export

nuts_get_data <- function(data){

  # CODE BREAKING CHECKS
  #------------------------
  # Input checks
  if (!(inherits(data, "nuts.classified")))
    cli_abort(
      "Input {.arg data} must be a nuts.classified-object, not {.obj_type_friendly {data}}."
    )

  # RETURN TIBBLE
  #------------------------

  return(data$data)
}
