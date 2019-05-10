
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
and creates one or multiple packages (one by country inputed) containing
the data and the documentation.

## Installation

You can install the development version of histgadm from
[GitHub](https://github.com/epix-project/histgadm) with:

``` r
# install.packages("devtools")
devtools::install_github("epix-project/histgadm")
```

## Usage

The `histgadm` contains four functions.

The function `init_package` creates a package for the GADM data
(<https://gadm.org/>), and interactively allows the user to download
data from GADM for one or more country and recreates historical map from
a time range (interactive input) by merging or spliting back admin1
polygons. The ouput are `sf` maps of the country in high and low
definition with the different admin1 historial and current
administrative boundaries but also the country administrative
boundaries. The historical map are “approximate” map because the package
only split or merge admin1 polygons when in reality, this event are
ofter more complex and the boundaries may not be exact.

Below is a quick look at how `histgadm` is working:

``` r
library("histgadm")

# Create a new package ---------------------------------------------------------
tmp <- file.path(tempdir(), "pkgtest")
dir.create(tmp)
initial_pkg(tmp, "test")
#> ✔ Creating '/var/folders/fp/845v7zc96rjdgs2l_86qgy5r0000gn/T/RtmpVD2j7z/pkgtest/test/'
#> ✔ Setting active project to '/private/var/folders/fp/845v7zc96rjdgs2l_86qgy5r0000gn/T/RtmpVD2j7z/pkgtest/test'
#> ✔ Creating 'R/'
#> ✔ Writing 'DESCRIPTION'
#> Package: test
#> Title: What the Package Does (One Line, Title Case)
#> Version: 0.0.0.9000
#> Authors@R (parsed):
#>     * First Last <first.last@example.com> [aut, cre] (<https://orcid.org/YOUR-ORCID-ID>)
#> Description: What the package does (one paragraph).
#> License: What license it uses
#> Depends:
#>     R (>= 2.10)
#> Imports:
#>     sf
#> Encoding: UTF-8
#> LazyData: true
#> ✔ Writing 'NAMESPACE'
#> ✔ Setting active project to '<no active project>'
#> 
#> Do you want to download GADM file from the internet? y / n (default)
#> 
#> Selection:
```

By default, the package created contained no data but if `y` is answer
the package will then ask the user to imput at least one country name
and a time range to download the data corresponding to gadm and to
recreate the historical map corresonding to the time range inputed.
Currently, it works only for `Cambodia, Laos, Thailand and Vietnam`.

For other country, the users will need to use the functions `map_data`,
`map_documentation` to complete their packages. For more details, please
take a look at the vignettes `histgadm Usage`.
