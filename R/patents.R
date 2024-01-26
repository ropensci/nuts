#' Patent applications to the EPO by priority year by NUTS 3 regions (pat_ep_rtot)
#'
#' The data frame contains information on patent applications to the European Patent Office
#' by year and NUTS 3 regions.
#'
#' @format ## `patents`
#' A data frame with 104,106 rows and 4 columns:
#' \describe{
#'   \item{unit}{4 indicators: Number, Nominal GDP in billion euro, Per million habitants,
#'   Per million of population in the labor force}
#'   \item{geo}{NUTS 1, 2, 3 or National level}
#'   \item{time}{Years 2008, 2009, 2010, 2011 and 2012}
#'   \item{values}{Values}
#' }
#' @source <https://ec.europa.eu/eurostat/databrowser/view/PAT_EP_RTOT/default/table>
"patents"
