.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  packageStartupMessage(
    "If you use this package in your work, please support the development of open science by citing:

", cli::col_blue("Hennicke M, Krause W, Batista e Silva F, Pigaiani C, Tucci M, Attardo C (2026). nuts: Convert European Regional Data. doi:10.5281/zenodo.10573056, R package version 1.3.0, <https://docs.ropensci.org/nuts/>"
    )
  )
}
