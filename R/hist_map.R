# ------------------------------------------------------------------------------
#' Thinning (simplification)
#'
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on a `sf`
#' object.
#'
#' @param sf_obj an objet of class "sf".
#' @param tolerance the tolerance value in the metric of the input object (cf.
#'  function `thinnedSpatialPoly`).
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
#' @param sf_obj an objet of class "sf".
#' @param boundbox character, bounding box.
#' @param crs character, coordinate reference system.
#' @keywords internal
#' @noRd
define_bbox_proj <- function(sf_obj, boundbox, crs) {
  attr(sf_obj[["geometry"]], "bbox") <- boundbox
  attr(sf_obj[["geometry"]], "crs") <- crs
  sf_obj
}

# ------------------------------------------------------------------------------
#' Translates vector to standardized English
#'
#' @param vector vector to translate
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See `Details` for more
#'  information.
#'
#' @importFrom stringi stri_escape_unicode
#' @keywords internal
#' @noRd
translate <- function(vector, hash = NULL) {
 vector %<>% stringi::stri_escape_unicode(.)
 if (is.null(hash) == FALSE) vector %<>% hash[.]
 vector
}

# ------------------------------------------------------------------------------
#' Filters list by a time range
#'
#' Filters a list to keep only the data corresponding to a certain time
#' range in year (between \code{to} and \code{from} (exclude)).
#'
#' @param hist_lst A list containing at least the variable \code{year}.
#' @param from Initial date of the time range, \code{character}, \code{numeric}
#' or of class \code{Date}. Select year after `from`.
#' @param to Final date of the data, \code{character}, \code{numeric} or of
#' class \code{Date}.
#' @return A list with the same variables as \code{}.hist_lst
#' @keywords internal
#' @noRd
select_events <- function(hist_lst, from, to) {
  sel0 <- map(hist_lst, "year") %>% unlist %>% as.Date()
  sel0 <- sel0 > as.Date(paste0(from, "-01-01")) &
    sel0 <= as.Date(paste0(to, "-12-31"))
  hist_lst <- hist_lst[sel0]
  hist_lst
}

# ------------------------------------------------------------------------------
#' Download current map for one country
#'
#' @param country character string, name of the country to download.
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See `Details` for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complexe merge/
#'  complexe split) and a slot year. See `Details` for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param d.hash used in case of `complexe split` or `complexe merge` in the
#'  `lst_history` object.  named character vector containing the translation in
#'  English (standardized version) of the admin2 names. See `Details` for more
#'  information.
#' @param path character string, name where the dowloaded file is saved.
#' @param file_rm boolean, if TRUE, remove the dowmloaded file.
#'   By default, TRUE.
#'
#' @importFrom sptools gadm
#' @keywords internal
#' @noRd
current_map <- function(country, hash, lst_history, from, to, d.hash,
                        path, file_rm) {

  # exception for Vietnam
  if (country == "Vietnam" & from <= 2007 ) {
    df_sf <- get("vn_a1_0407")  %>%
      mutate(province = translate(NAME_2, hash)) %>%
      select(province, geometry)

  } else if (is.null(lst_history) == FALSE &&
             select_events(lst_history, from, to) %>% map("event") %>%
               grepl("complexe", .) %>% any) {
      df_sf <- gadm(country, "sf", 2, path = path, file_rm = file_rm) %>%
        mutate(province = translate(NAME_1, hash),
               district = translate(NAME_2, d.hash)) %>%
        select(province, district, geometry)
  } else {
    df_sf <- gadm(country, "sf", 1, path = path, file_rm = file_rm) %>%
      mutate(province = translate(NAME_1, hash)) %>%
      select(province, geometry)
  }
  df_sf
}

# ------------------------------------------------------------------------------
#' Tests and selects map
#'
#' Test and select map with the spatial expression corresponding to the time
#' frame selected.
#'
#' @param lst A list containing sf object containing two columns: `geometry` and
#'   `province` , with named slot : `XX_YEAR_YEAR_QUALITY`, XX is the country
#'   name in two letters code.
#' @param test_lst  A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time.
#'
#' @importFrom sptools gadm
#' @keywords internal
#' @noRd
sel_map <- function(lst, test_lst) {
  test <- lst %>%
      map(as.data.frame) %>%
      map(select, -"geometry") %>%
      discard(grepl("country", names(.))) %>%
      map(dictionary::match_pattern, "province", test_lst) %>%
      setNames(names(.) %>% gsub("^.._", "", .) %>% gsub("_.{3,4}$", "", .)) %>%
      map(stringr::str_replace, "-", "_")
  sel1 <- names(test[which(substr(test, 1, 4) != names(test) %>% substr(1, 4))])
  sel2 <- names(test[which(substr(test, 6, 9) != names(test) %>% substr(6, 9))])
  sel <- intersect(sel1, sel2)
  if (length(sel) != 0) {
    total_lst <- discard(lst, grepl(sel %>% paste(collapse = "|"), names(lst)))
  } else {
    total_lst <- lst
  }
  total_lst
}

# ------------------------------------------------------------------------------
#' Create a list of historical map
#'
#' From a time range (by default: 1960-01-01 / 2020-12-31), recreates old map
#' by merging back together or spliting admin1 polygons from the current admin1
#' administrative boundaries downloaded from GADM \url{https://gadm.org}. Two
#' maps will be create for each year of event (split, merge or rename of
#' admin1), one in high resolution and one in low resolution.
#'
#' @details The functions  needs a named vector, `hash` and `d.hash`
#' arguments, to translate the `NAME_1` column (and `NAME_2` if necessary) from
#' GADM \url{https://gadm.org} in a standardized English version. We advice to
#' use the named vector `xx_province` for admin1 or `xx_district` for admin2
#' contained in the `dictionary` package, for example:
#' \code{\link[dictionary]{kh_province}}. If no `hash` and/or `d.hash` arguments
#' is missing the column(s) `NAME_1` and/or `NAME_2` are encoded in UNICODE and
#' keep in native language.
#' \cr\cr
#' The function needs also a list of event (split/merge/rename/
#' complexe merge/complexe split) in a standardized format to recreate
#' historical map. We advice to use or the copy the format of the list
#' `xx_history` contained in the package `dictionary`.
#' For example: \code{\link[dictionary]{kh_history}}.
#' If no list are inputed in the `lst_history` argument, only the current map of
#' admin1 administrative boundary and the country boundary in high and low
#' resolution in a list will be created.
#' \cr\cr
#' The package `dictionary` is available on GitHub, to install it, it necessary
#' to have the `devtools` package:
#' `devtools::install_github("choisy/dictionary")`
#' \cr\cr
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on
#' each map object with the tolerance (argument `tolerance`) value in the metric
#' of the input object.
#' \cr\cr
#' The arguments `lst_province_year` should be input as a list of charactor
#' vector containing the names of the admin1 written in a same way as `hash`
#' and/or `lst_history` ordered by year of change in administrative boundaries.
#' We advice to use or the copy the format of the list `xx_province_year`
#' contained in the package `dictionary`. For example:
#' \code{\link[dictionary]{kh_province_year}}.
#' \cr\cr
#' The output of the function is a named list: the admin1 boundaries named are
#' named as: the 2 characters ISO code, the year of expression of this admin1
#' administrative boundaries and the quality. For example: "vn_1997_2004_high"
#' for the admin1 boundaries of Vietnam from 1997-01-01 until 2004-01-01 (not
#' include) in high quality.
#'
#' @param country character string, name of the country to download.
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See `Details` for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complexe merge/
#'  complexe split) and a slot year. See `Details` for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020".
#' @param d.hash used in case of `complexe split` or `complexe merge` in the
#'  `lst_history` object.  named character vector containing the translation in
#'  English (standardized version) of the admin2 names. See `Details` for more
#'  information.
#' @param tolerance numeric for thinning (simplification). the tolerance value
#'  should be in the metric of the input object (cf. from function
#'  \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = 0.01.
#' @param path character string, name where the dowloaded file is saved.
#' @param file_rm boolean, if TRUE, remove the dowmloaded file.
#'   By default, TRUE.
#' @param lst_province_year A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time. See `Details` for more inforamtion
#'
#' @return list of `sf` object containing the maps of admin1 administrative
#' boundaries and two maps of the country boundaries (one in high resolution
#' and one in low resolution).
#'
#' @examples
#' library(dictionary)
#'
#' kh_map <- hist_map("Cambodia", kh_province, kh_history)
#'
#' @importFrom dplyr mutate select rename
#' @importFrom sf st_crs st_bbox
#' @importFrom sptools sf_aggregate_lst
#' @importFrom purrr map flatten discard
#' @importFrom lubridate year
#' @importFrom stats setNames
#' @importFrom dictionary match_pattern
#' @importFrom stringr str_replace
#'
#' @export
hist_map <- function(country, hash, lst_history, from = "1960",
                      to = "2020", d.hash = NULL, tolerance = 0.01,
                      path = NULL, file_rm = FALSE, lst_province_year = NULL) {

  if (missing(hash)) hash <- NULL
  if (missing(lst_history)) lst_history <- NULL

  # ACTUAL MAP
  df_sf <- current_map(country = country, hash = hash,
                       lst_history = lst_history, from = from, to = to,
                       d.hash = d.hash, path = path, file_rm = file_rm)

  boundbox <- st_bbox(df_sf)
  crs <- st_crs(df_sf)

  # COUNTRY
  gadm0r <- gadm(country, "sf", 0, path = path, file_rm = file_rm) %>%
    select(-GID_0) %>%
    rename(country = NAME_0) %>%
    define_bbox_proj(boundbox, crs)
  gadm0 <- thin_polygons(gadm0r, tolerance = tolerance) %>%
    define_bbox_proj(boundbox, crs)

  # SELECT THE YEARS & MAKE THE LIST OF OLD MAP
  from <-  paste0(from, "-01-01") %>% as.Date()
  to <- paste0(to, "-12-31") %>% as.Date()
  if (is.null(lst_history)) {

    sel_year <- NULL
    total_lst <- list(list(df_sf,
                      df_sf %<>%
                        thin_polygons(tolerance = tolerance) %>%
                        define_bbox_proj(boundbox, crs)) %>%
                        setNames(c("high", "low"))) %>%
      setNames(Sys.time() %>% lubridate::year(.))

  } else {

    sel_year <- lst_history %>% map("year") %>% map(as.Date) %>%
      c(from, .) %>% unlist %>% unique %>% .[which(. < to & . >= from)] %>%
      lubridate::year(.)

    total_lst <- lapply(seq_along(sel_year), function (x) {

      if (country == "Vietnam" & sel_year[x] >= "2008") {
        old_mapr <- gadm(country, "sf", 1, path = path, file_rm = file_rm) %>%
          mutate(province = translate(NAME_1, hash)) %>%
          select(province, geometry)
      } else {
        old_mapr <- sf_aggregate_lst(df_sf, lst_history, from = sel_year[x]) %>%
         define_bbox_proj(boundbox, crs)
      }
      print(old_mapr)
      old_map <- thin_polygons(old_mapr, tolerance = tolerance) %>%
        define_bbox_proj(boundbox, crs)
      list(old_mapr, old_map) %>% setNames(c("high", "low"))
    }) %>%
      setNames(sel_year %>% paste(c(sel_year[-1], lubridate::year(to) + 1),
                                  sep = "_"))
  }
  # APPEND COUNTRY MAP
  total_lst %<>% append(list(list(country = gadm0r, gadm0) %>%
                               setNames(c("high", "low"))) %>%
                          setNames("country"))

  # MAKE NAME FILE
  name <- lapply(seq_along(total_lst), function(x) {
    total_lst[[names(total_lst)[x]]] %>% names %>%
      paste0(
        countrycode::countrycode(country, "country.name", "iso2c") %>% tolower,
        "_", names(total_lst)[x], "_", .)
  }) %>%
    unlist
  total_lst %<>% flatten(.) %>% setNames(name)

  if (is.null(lst_province_year) == FALSE){
    total_lst <- sel_map(total_lst, lst_province_year)
  }
 total_lst
}

## quiets concerns of R CMD check for the values that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "GID_0", "NAME_0",
                                                        "NAME_1", "NAME_2",
                                                        "district", "geometry",
                                                        "province"))
