
<!-- README.md is generated from README.Rmd. Please edit that file -->

# histgadm

[![AppVeyor build
status](https://ci.appveyor.com/api/pruojects/status/github/epix-project/histgadm?branch=master&svg=true)](https://ci.appveyor.com/project/epix-project/histgadm)
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
ofter more complexe and the boundaries may not be exact.

Below is a quick look at how `histgadm` is working:

``` r
library("histgadm")

# Create a new package ---------------------------------------------------------
tmp <- file.path(tempdir(), "pkgtest")
dir.create(tmp)
initial_pkg(tmp, "test")
#> ✔ Setting active project to '/private/var/folders/fp/845v7zc96rjdgs2l_86qgy5r0000gn/T/RtmpfaKATm/pkgtest/test'
#> ✔ Creating 'R/'
#> ✔ Creating 'man/'
#> ✔ Writing 'DESCRIPTION'
#> ✔ Writing 'NAMESPACE'
#> ✔ Adding 'sf' to Imports field in DESCRIPTION
#> ● Refer to functions with `sf::fun()`
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
`map_documentation` to complete their packages. If the user want to
recreate historical map, it will need to input a list containing the
historical event and a dictionary for the admin1, admin2 names. For now,
the package `dictionary` contains functions to create them but also
dictionary and list of event for Cambodia, Laos, Thailand and Vietnam.

To install it:

``` r
# install.packages("devtools")
devtools::install_github("choisy/dictionary")
```

Let’s now take a look at `map_data`: it download the current map from
GADM and recreates historical map:

``` r
pgkg_dir <- paste0(tmp, "/test")
map_data(pgkg_dir, "Cambodia", dictionary::kh_province, dictionary::kh_history, from = "1980", to = "2018")
#> ✔ Creating 'data-raw/'
#> ✔ Adding '^data-raw$' to '.Rbuildignore'
#> Next:
#> ● Add data creation scripts in 'data-raw/'
#> ● Use `usethis::use_data()` to add data to package
#> Simple feature collection with 23 features and 1 field
#> geometry type:  GEOMETRY
#> dimension:      XY
#> bbox:           xmin: 102.3355 ymin: 9.91361 xmax: 107.6303 ymax: 14.68817
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#> # A tibble: 23 x 2
#>    province                                                       geometry
#>    <chr>                                                    <GEOMETRY [°]>
#>  1 Banteay Meanc… POLYGON ((103.4153 13.55372, 103.4148 13.55336, 103.414…
#>  2 Batdambang     POLYGON ((102.5209 12.66142, 102.52 12.66127, 102.5192 …
#>  3 Kampong Cham   POLYGON ((106.2983 11.6803, 106.2984 11.68044, 106.2984…
#>  4 Kampong Chhna… MULTIPOLYGON (((104.7265 11.81627, 104.7259 11.81546, 1…
#>  5 Kampong Spoe   POLYGON ((104.4795 11.1239, 104.4795 11.12367, 104.4789…
#>  6 Kampong Thum   POLYGON ((105.1992 12.21036, 105.1996 12.20916, 105.200…
#>  7 Kampot         MULTIPOLYGON (((104.4399 10.4224, 104.4398 10.42238, 10…
#>  8 Kandal         POLYGON ((105.1889 10.91159, 105.189 10.91179, 105.1809…
#>  9 Kaoh Kong      MULTIPOLYGON (((102.8862 9.931321, 102.8865 9.931264, 1…
#> 10 Kep            POLYGON ((104.3426 10.49478, 104.3422 10.4945, 104.3419…
#> # ... with 13 more rows
#> Simple feature collection with 24 features and 1 field
#> geometry type:  GEOMETRY
#> dimension:      XY
#> bbox:           xmin: 102.3355 ymin: 9.91361 xmax: 107.6303 ymax: 14.68817
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#> # A tibble: 24 x 2
#>    province                                                       geometry
#>    <chr>                                                    <GEOMETRY [°]>
#>  1 Banteay Meanc… POLYGON ((103.4153 13.55372, 103.4148 13.55336, 103.414…
#>  2 Batdambang     POLYGON ((102.9906 12.47703, 102.9901 12.47669, 102.989…
#>  3 Kampong Cham   POLYGON ((106.2983 11.6803, 106.2984 11.68044, 106.2984…
#>  4 Kampong Chhna… MULTIPOLYGON (((104.7265 11.81627, 104.7259 11.81546, 1…
#>  5 Kampong Spoe   POLYGON ((104.4795 11.1239, 104.4795 11.12367, 104.4789…
#>  6 Kampong Thum   POLYGON ((105.1992 12.21036, 105.1996 12.20916, 105.200…
#>  7 Kampot         MULTIPOLYGON (((104.4399 10.4224, 104.4398 10.42238, 10…
#>  8 Kandal         POLYGON ((105.1889 10.91159, 105.189 10.91179, 105.1809…
#>  9 Kaoh Kong      MULTIPOLYGON (((102.8862 9.931321, 102.8865 9.931264, 1…
#> 10 Kep            POLYGON ((104.3426 10.49478, 104.3422 10.4945, 104.3419…
#> # ... with 14 more rows
#> Simple feature collection with 25 features and 1 field
#> geometry type:  GEOMETRY
#> dimension:      XY
#> bbox:           xmin: 102.3355 ymin: 9.91361 xmax: 107.6303 ymax: 14.68817
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#> # A tibble: 25 x 2
#>    province                                                       geometry
#>    <chr>                                                    <GEOMETRY [°]>
#>  1 Banteay Meanc… POLYGON ((103.4153 13.55372, 103.4148 13.55336, 103.414…
#>  2 Batdambang     POLYGON ((102.9906 12.47703, 102.9901 12.47669, 102.989…
#>  3 Kampong Cham   POLYGON ((105.4508 11.86398, 105.4505 11.86166, 105.450…
#>  4 Kampong Chhna… MULTIPOLYGON (((104.7265 11.81627, 104.7259 11.81546, 1…
#>  5 Kampong Spoe   POLYGON ((104.4795 11.1239, 104.4795 11.12367, 104.4789…
#>  6 Kampong Thum   POLYGON ((105.1992 12.21036, 105.1996 12.20916, 105.200…
#>  7 Kampot         MULTIPOLYGON (((104.4399 10.4224, 104.4398 10.42238, 10…
#>  8 Kandal         POLYGON ((105.1889 10.91159, 105.189 10.91179, 105.1809…
#>  9 Kaoh Kong      MULTIPOLYGON (((102.8862 9.931321, 102.8865 9.931264, 1…
#> 10 Kep            POLYGON ((104.3426 10.49478, 104.3422 10.4945, 104.3419…
#> # ... with 15 more rows
#> ✔ Saving 'kh_1980_1997_high', 'kh_1980_1997_low', 'kh_1997_2013_high', 'kh_1997_2013_low', 'kh_2013_2018_high', 'kh_2013_2018_low', 'kh_country_high', 'kh_country_low' to 'data/kh_1980_1997_high.rda', 'data/kh_1980_1997_low.rda', 'data/kh_1997_2013_high.rda', 'data/kh_1997_2013_low.rda', 'data/kh_2013_2018_high.rda', 'data/kh_2013_2018_low.rda', 'data/kh_country_high.rda', 'data/kh_country_low.rda'
```

And the function `map_documentation` will create the documentation for
each object.

``` r
map_documentation(pgkg_dir)
#> Updating roxygen version in /private/var/folders/fp/845v7zc96rjdgs2l_86qgy5r0000gn/T/RtmpfaKATm/pkgtest/test/DESCRIPTION
#> Writing NAMESPACE
#> Loading test
#> Writing kh_1980_1997_high.Rd
#> Writing kh_1980_1997_low.Rd
#> Writing kh_1997_2013_high.Rd
#> Writing kh_1997_2013_low.Rd
#> Writing kh_2013_2018_high.Rd
#> Writing kh_2013_2018_low.Rd
#> Writing kh_country_high.Rd
#> Writing kh_country_low.Rd
```

For more information, please take a look at the vignettes.
