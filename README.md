# nuts <img src="man/figures/logo.png" align="right" height="200"/>

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/AAoritz/nuts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AAoritz/nuts/actions/workflows/R-CMD-check.yaml)
  [![codecov](https://codecov.io/gh/AAoritz/nuts/graph/badge.svg?token=UXSUFJOD2U)](https://codecov.io/gh/AAoritz/nuts)
  [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
  [![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/623_status.svg)](https://github.com/ropensci/software-review/issues/623)
  <!-- badges: end -->

Changing administrative regional boundaries over time complicates the construction of consistent regional panel data. The `nuts` package can transform **European regional data** identified by **NUTS codes** between different **versions** and **subdivision levels** with the aim to harmonize and link data sets. 

The package's main task is to convert data between all NUTS versions in use: 2006, 2010, 2013, 2016 and 2021. These conversions can be performed in any direction based on **spatial interpolation** using five different weights (regional area in square kilometers, 2011 and 2018 population size, 2012 and 2018 built-up area). The weights are based on conversion tables provided by the [Joint Research Center (JRC) of the European Commission (EC)](<https://urban.jrc.ec.europa.eu/nutsconverter/#/>) that are used in their online converter. The `nuts` package permits to do the same conversion offline with additional advantages such as the conversion within groups, enabling the conversion of datasets with multiple NUTS versions efficiently.

### Installation

You can install the most recent development version from Github:

``` r
pak::pak("AAoritz/nuts/")
```

### Usage

Check out the [website](https://aaoritz.github.io/nuts/articles/nuts-vignette.html) for example usage and further details.

### Citation

Please support the development of open science and data by citing the JRC and us in your work:

-   Joint Research Centre (2022) NUTS converter. <https://urban.jrc.ec.europa.eu/nutsconverter>

-   Hennicke M, Krause W (2024). _nuts: Convert European Regional Data_. R package version 0.0.0.9000, <https://AAoritz.github.io/nuts/>.


Bibtex Users:

```         
@Manual{,
  title = {NUTS converter},
  author = {Joint Research Centre},
  year = {2022},
  url = {https://urban.jrc.ec.europa.eu/nutsconverter},
}

@Manual{,
  title = {nuts: Convert European Regional Data},
  author = {Moritz Hennicke and Werner Krause},
  year = {2024},
  note = {R package version 0.0.0.9000},
  url = {https://AAoritz.github.io/nuts/},
}
```
