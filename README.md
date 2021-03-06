
<!-- README.md is generated from README.Rmd. Please edit that file -->

# histgadm

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/8a3jtxe4uiehushs/branch/master?svg=true)](https://ci.appveyor.com/project/epixproject/histgadm-cs9wa/branch/master)
[![Travis build
status](https://travis-ci.org/epix-project/histgadm.svg?branch=master)](https://travis-ci.org/epix-project/histgadm)
[![Coverage
status](https://codecov.io/gh/epix-project/histgadm/branch/master/graph/badge.svg)](https://codecov.io/github/epix-project/histgadm?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/histgadm)](https://cran.r-project.org/package=histgadm)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`histgadm` is a workflow package: From current map from GADM
(<https://gadm.org/>) recreates historical administrative boundaries map
and can also create one package containing the data and the
documentation.

## Installation

You can install the development version of histgadm from
[GitHub](https://github.com/epix-project/histgadm) with:

``` r
# install.packages("devtools")
devtools::install_github("epix-project/histgadm")
```

## Usage

The `histgadm` contains four functions that works together. The function
`hist_map` creates historical map, the functions `map_data` &
`map_documentation` call the function `hist_map` in the environment of a
package and stock the results (maps in `.rda` files and documentation)
and download file in the folder corresponding. The function
`init_package` called the three functions to create interactively with
the user a package containing data, source files and documentation.
Please take a look at the vignette for more information.
