#' Conversion table provided by the Joint Research Center of the European Commission
#'
#' The table contains population, area and surface flows between two NUTS regions and
#' different NUTS code classifications. NUTS regions are at 1st, 2nd and 3rd level.
#' NUTS versions are 2006, 2010, 2013, 2016, 2021 and 2024.
#'
#' @format ## `cross_walks`
#' A data frame with 66,877 rows and 12 columns:
#' \describe{
#'   \item{from_code}{Departing NUTS code}
#'   \item{to_code}{Desired NUTS code}
#'   \item{from_version}{Departing NUTS version}
#'   \item{to_version}{Desired NUTS version}
#'   \item{level}{NUTS division level}
#'   \item{country}{Country name}
#'   \item{areaKm}{Area size flow}
#'   \item{pop21}{2021 population flow}
#'   \item{pop11}{2011 population flow}
#'   \item{artif_surf18}{2018 artificial surfaces flow}
#'   \item{artif_surf12}{2012 artificial surfaces flow}
#'   \item{bu_vol}{2021 residential built-up volume flow}
#' }
#' @source <https://urban.jrc.ec.europa.eu/tools/nuts-converter?lng=en#/>
"cross_walks"
