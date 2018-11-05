# ------------------------------------------------------------------------------
#' Thinning (simplification)
#'
#'  The function performs \code{\link[maptools]{thinnedSpatialPoly}} on a `sf`
#'  object.
#'
#' @param sf_obj an objet of class "sf"
#' @param tolerance the tolerance value in the metric of the input object (cf.
#'  function `thinnedSpatialPoly`)
#'
#' @importFrom sf as_Spatial st_as_sf
#' @importFrom magrittr %>% %<>%
#' @importFrom maptools thinnedSpatialPoly
#' @keywords internal
#' @noRd
thin_polygons <- function(sf_obj, tolerance) {
  sf_obj %<>% as_Spatial(.) %>%
    thinnedSpatialPoly(tolerance) %>%
    st_as_sf(.)
}

# ------------------------------------------------------------------------------
#' Defines new boundaries box and projections of a sf object
#'
#' The function defines the attributes `bbox` and `crs` of a sf object.
#'
#' @param sf_obj an objet of class "sf"
#' @param boundbox character, bounding box
#' @param crs character, coordinate reference system
#' @keywords internal
#' @noRd
define_bbox_proj <- function(sf_obj, boundbox, crs) {
  attr(sf_obj[["geometry"]], "bbox") <- boundbox
  attr(sf_obj[["geometry"]], "crs") <- crs
  sf_obj
}

# ------------------------------------------------------------------------------
#' Create a list of historical map
#'
#' From a time range (by default: 1960-01-01 / 2020-12-31), recreates old map
#' by merging back together or spliting admin1 polygons. Two maps will be create
#' for each year of event (split, merge or rename of admin1), one in high
#' resolution and one in low resolution.
#'
#' @details The functions requires a named vector, `hash` and `d.hash` arguments,
#' to translate the `NAME_1` column (and `NAME_2` if necessary) from GADM
#' \url{https://gadm.org} in a standardized English version. We advice to use
#' the named vector `xx_province` for admin1 or `xx_district` for admin2
#' contained in the `dictionary` package, for example:
#' \code{\link[dictionary]{kh_province}}.
#' \cr\cr
#' The function requires also a list of event (split/merge/rename/
#' complexe merge/complexe split) in a standardized format to recreate
#' historical map. We advice to use or the copy the format of the list
#' `xx_history` contained in the package `dictionary`.
#' For example: \code{\link[dictionary]{kh_history}}.
#' \cr\cr
#' The package `dictionary` is available on GitHub, to install it, it necessary
#' to have the `devtools` package:
#' `devtools::install_github("choisy/dictionary")`
#' \cr\cr
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on
#' each map object with the tolerance (argument `tolerance`) value in the metric
#' of the input object.
#'
#' @param country character string, name of the country to download
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See `Details` for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complexe merge/
#'  complexe split) and a slot year. See `Details` for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020"
#' @param d.hash used in case of `complexe split` or `complexe merge` in the
#'  `lst_history` object.  named character vector containing the translation in
#'  English (standardized version) of the admin2 names. See `Details` for more
#'  information.
#' @param tolerance numeric for thinning (simplification). the tolerance value
#'  should be in the metric of the input object (cf. from function
#'  \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = 0.01.
#'
#' @return list of `sf` object containing the maps of admin1 administrative
#' boundaries and two maps of the country boundaries (one in high resolution
#' and one in low resolution).
#'
#' @examples
#'
#' library(dictionary)
#'
#' kh_map <- hist_gadm("Cambodia", kh_province, kh_history)
#'
#' @importFrom dplyr mutate select rename
#' @importFrom stringi stri_escape_unicode
#' @importFrom sf st_crs st_bbox
#' @importFrom sptools gadm sf_aggregate_lst
#' @importFrom purrr map flatten
#' @importFrom lubridate year
#' @importFrom stats setNames
#'
#' @export
hist_gadm <- function(country, hash, lst_history, from = "1960",
                           to = "2020", d.hash = NULL, tolerance = 0.01) {
  # ACTUAL MAP
  # exception for Vietnam
  if (country == "Vietnam" & from <= 2007 ) {
    df_sf <- readRDS("data-raw/gadm_vn_0407.rds")  %>%
      mutate(province = stringi::stri_escape_unicode(NAME_2) %>%
               hash[.]) %>%
      select(province, geometry)
    current_map <- gadm(country, "sf", 1) %>%
      mutate(province = stringi::stri_escape_unicode(NAME_1) %>%
               hash[.]) %>%
      select(province, geometry)
  } else if (lst_history %>% map("event") %>% grepl("complexe", .) %>% any){
    df_sf <- gadm(country, "sf", 2) %>%
      mutate(province = stringi::stri_escape_unicode(NAME_1) %>%
               hash[.],
             district = stringi::stri_escape_unicode(NAME_2) %>%
               d.hash[.]) %>%
      select(province, district, geometry)
  } else {
    df_sf <- gadm(country, "sf", 1) %>%
      mutate(province = stringi::stri_escape_unicode(NAME_1) %>%
               hash[.]) %>%
      select(province, geometry)
  }
  boundbox <- st_bbox(df_sf)
  crs <- st_crs(df_sf)

  # COUNTRY
  gadm0r <- gadm(country, "sf", 0) %>% select(-GID_0) %>%
    rename(country = NAME_0) %>%
    define_bbox_proj(boundbox, crs)
  gadm0 <- thin_polygons(gadm0r, tolerance = tolerance) %>%
    define_bbox_proj(boundbox, crs)

  # SELECT THE YEARS
  from <-  paste0(from, "-01-01") %>% as.Date()
  to <- paste0(to, "-12-31") %>% as.Date()
  sel_year <- lst_history %>% purrr::map("year") %>% purrr::map(as.Date) %>%
    c(from, .) %>% unlist %>% unique %>% .[which(. < to & . >= from)] %>%
    lubridate::year(.)
  print(sel_year)

  # MAKE THE LIST OF OLD MAP
  total_lst <- lapply(seq_along(sel_year), function (x) {
    if (country == "Vietnam" & sel_year[x] >= "2008") {
      old_mapr <- current_map %>% define_bbox_proj(boundbox, crs)
    } else {
      old_mapr <- sf_aggregate_lst(df_sf, lst_history, from = sel_year[x]) %>%
        define_bbox_proj(boundbox, crs)
    }
    old_map <- thin_polygons(old_mapr, tolerance = tolerance) %>%
      define_bbox_proj(boundbox, crs)
    list(old_mapr, old_map) %>% setNames(c("high", "low"))
  }) %>%
    setNames(sel_year %>% paste(c(sel_year[-1], lubridate::year(to)),
                                sep = "_")) %>%
    append(list(list(country = gadm0r, gadm0) %>%
                  setNames(c("high", "low"))) %>%
             setNames("country"))

  name <- lapply(seq_along(total_lst), function(x) {
    total_lst[[names(total_lst)[x]]] %>% names %>%
      paste(names(total_lst)[x], ., sep = "_")
  }) %>%
    unlist
  total_lst %<>% flatten(.) %>% setNames(name)
}

## quiets concerns of R CMD check for the values that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "GID_0", "NAME_0",
                                                        "NAME_1", "NAME_2",
                                                        "district", "geometry",
                                                        "province"))

