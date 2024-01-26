#' Manure storage facilities by NUTS 3 regions from Eurostat (aei_fm_ms)
#'
#' The data frame contains the number of different manure storage facilities from the Farm Structure Survey
#' in all (former) EU member states, such as Iceland, Norway, Switzerland and Montenegro at the NUTS 3 level.
#' Please see the link indicated below for more information.
#'
#' @format ## `manure`
#' A data frame with 17,151 rows and 4 columns:
#' \describe{
#'   \item{indic_ag}{9 indicators: All manure storage facilities, solid dung, liquid manure
#'   slurry, slurry: tank, slurry: lagoon; covered facilities with either dung, liquid manure, slurry}
#'   \item{geo}{NUTS 1, 2, 3 or National level}
#'   \item{time}{Years 2000, 2003 and 2010}
#'   \item{values}{Number}
#' }
#' @source <https://ec.europa.eu/eurostat/databrowser/view/aei_fm_ms/default/table?lang=en>
"manure"
