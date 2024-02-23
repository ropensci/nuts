#' Return missing NUTS codes in classified NUTS data
#'
#' `nuts_get_missing()` returns the classified data after running `nuts_classify()`.
#'
#' @param data A nuts.classified object returned by [`nuts_classify()`].
#'
#' @details Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and
#' `nuts.verbose = "verbose"` to switch messages back on.
#'
#' @return A tibble listing missing NUTS codes for each group.
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
#' nuts_get_missing(classified)
#'
#' @export

nuts_get_missing <- function(data){

  # CODE BREAKING CHECKS
  #------------------------
  # Input checks
  if (!(inherits(data, "nuts.classified")))
    cli_abort(
      "Input {.arg data} must be a nuts.classified-object, not {.obj_type_friendly {data}}."
    )

  # RETURN TIBBLE
  #------------------------
  return(data$missing)

}
