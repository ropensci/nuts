#' Return version overlap of classified NUTS data
#'
#' `nuts_get_version()` returns the classified data after running `nuts_classify()`.
#'
#' @param data A nuts.classified object returned by [`nuts_classify()`].
#'
#' @details Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and
#' `nuts.verbose = "verbose"` to switch messages back on.
#'
#' @return A tibble that lists the group-specific overlap with each NUTS version.
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
#' nuts_get_version(classified)
#'
#' @export

nuts_get_version <- function(data){

  # CODE BREAKING CHECKS
  #------------------------
  # Input checks
  if (!(inherits(data, "nuts.classified")))
    cli_abort(
      "Input {.arg data} must be a nuts.classified-object, not {.obj_type_friendly {data}}."
    )

  # RETURN TIBBLE
  #------------------------
  return(data$version)

}
