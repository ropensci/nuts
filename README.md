# nuts <img src="man/figures/logo.png" align="right" height="200"/>

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/ropensci/nuts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/nuts/actions/workflows/R-CMD-check.yaml)
  [![Codecov test coverage](https://codecov.io/gh/ropensci/nuts/graph/badge.svg)](https://app.codecov.io/gh/ropensci/nuts)
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/155_status.svg)](https://github.com/ropensci/software-review/issues/623)
  [![DOI](https://zenodo.org/badge/748601068.svg)](https://zenodo.org/doi/10.5281/zenodo.10573056)
  <!-- badges: end -->

Changing administrative regional boundaries over time complicates the construction of consistent regional panel data. The `nuts` package can transform **European regional data** identified by **NUTS codes** between different **versions** and **subdivision levels** with the aim to harmonize and link data sets. 

The package's main task is to convert data between all NUTS versions in use: 2006, 2010, 2013, 2016, 2021 and 2024. These conversions can be performed in any direction based on **spatial interpolation** using six different weights (regional area in square kilometers, 2011 and 2021 population size, 2012 and 2018 artificial surfaces and 2021 residential built-up volume). The weights are based on conversion tables provided by the [Joint Research Center (JRC) of the European Commission (EC)](https://urban.jrc.ec.europa.eu/tools/nuts-converter) that are used in their online converter. The `nuts` package permits to do the same conversion offline with additional advantages such as the conversion within groups, enabling the conversion of datasets with multiple NUTS versions efficiently.

### Installation

You can install the current development version from GitHub:
  
``` r
# install.packages("remotes")
remotes::install_github("ropensci/nuts")
```

### Usage

Check out the [website](https://docs.ropensci.org/nuts/articles/nuts.html) how to get started.

### Verbosity Control

Console messages can be controlled with `rlang::local_options(nuts.verbose = "quiet")` to silence messages and setting `nuts.verbose = "verbose"` to switch messages back on.


### Citation

Please support the development of open science and data by citing the JRC and us in your work:

-   Joint Research Centre (2022) NUTS converter. <https://urban.jrc.ec.europa.eu/tools/nuts-converter>

-   Hennicke M, Krause W (2024). _nuts: Convert European Regional Data_. doi:10.5281/zenodo.10573056
  <https://doi.org/10.5281/zenodo.10573056>, R package version 1.1.0, <https://docs.ropensci.org/nuts/>. 


Bibtex Users:

```         
@Manual{,
  title = {NUTS converter},
  author = {Joint Research Centre},
  year = {2022},
  url = {https://urban.jrc.ec.europa.eu/tools/nuts-converter},
}

@Manual{,
  title = {nuts: Convert European Regional Data},
  author = {Moritz Hennicke and Werner Krause},
  year = {2024},
  note = {R package version 1.1.0},
  url = {https://docs.ropensci.org/nuts/},
  doi = "https://doi.org/10.5281/zenodo.10573056",
}
```
