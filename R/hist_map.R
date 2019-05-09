# ------------------------------------------------------------------------------
#' Thinning (simplification)
#'
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on a
#' \code{sf} object.
#'
#' @param sf_obj an objet of class "sf".
#' @param tolerance the tolerance value in the metric of the input object (cf.
#'  function `thinnedSpatialPoly`).
#'
#' @importFrom sf as_Spatial st_as_sf
#' @importFrom maptools thinnedSpatialPoly
#' @keywords internal
#' @noRd
thin_polygons <- function(sf_obj, tolerance) {
  sf_obj <- as_Spatial(sf_obj)
  sf_obj <- thinnedSpatialPoly(sf_obj, tolerance)
  sf_obj <- st_as_sf(sf_obj)
}

# ------------------------------------------------------------------------------
#' Defines new boundaries box and projections of a sf object
#'
#' The function defines the attributes \code{bbox} and \code{crs} of a sf
#' object.
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
#'  (standardized version) of the admin1 names. See \code{Details}for more
#'  information.
#'
#' @importFrom stringi stri_escape_unicode
#' @keywords internal
#' @noRd
translate <- function(vector, hash = NULL) {
 vector <- stringi::stri_escape_unicode(vector)
 if (is.null(hash) == FALSE) vector <- hash[vector]
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
#' or of class \code{Date}. Select year after \code{from}.
#' @param to Final date of the data, \code{character}, \code{numeric} or of
#' class \code{Date}.
#' @return A list with the same variables as \code{}.hist_lst
#' @keywords internal
#' @noRd
select_events <- function(hist_lst, from, to) {
  sel0 <- unlist(lapply(hist_lst, "[", "year"))
  sel0 <- as.Date(sel0)
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
#'  (standardized version) of the admin1 names. See \code{Details} for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complexe merge/
#'  complexe split) and a slot year. See \code{Details} for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param d.hash used in case of \code{complexe split} or \code{complexe merge}
#' in the \code{lst_history} object.  named character vector containing the
#' translation in English (standardized version) of the admin2 names. See
#' \code{Details} for more information.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a specific path or not. If \code{NULL}, it will be asked interactively.
#' @param path character string, path to save the downloaded file. If
#' \code{NULL}, the file will be saved in the working directory.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' @param force boolean, force to download the file even if already in the path.
#' By default \code{FALSE}.
#'
#' @importFrom sptools gadm
#' @keywords internal
#' @noRd
current_map <- function(country, hash, lst_history, from, to, d.hash, save,
                        path, intlib, force) {

  if (!is.null(lst_history)) {
    event_history <- select_events(lst_history, from, to)
    event_history <- lapply(event_history, "[[", "event")
  }

  # exception for Vietnam
  if (country == "Vietnam" &
      as.Date(paste0(from, "-01-01")) < as.Date("2008-01-01")) {
    df_sf <- get("vn_a1_0407")
    df_sf <- transform(df_sf, province = translate(df_sf$NAME_2, hash))
    df_sf <- df_sf[, c("province", "geometry")]

  } else if (!is.null(lst_history) &&
             any(grepl("complexe", event_history))) {
      df_sf <- gadm(country, "sf", 2, save = save, path = path,
                    intlib = intlib, force = force)
      df_sf <- transform(df_sf, province = translate(df_sf$NAME_1, hash),
                         district = translate(df_sf$NAME_2, d.hash))
      df_sf <- df_sf[, c("province", "district", "geometry")]

  } else {
    df_sf <- gadm(country, "sf", 1, save = save, path = path,
                  intlib = intlib, force = force)
    df_sf <- transform(df_sf, province = translate(df_sf$NAME_1, hash))
    df_sf <- df_sf[, c("province", "geometry")]
  }
  st_as_sf(df_sf)
}

# ------------------------------------------------------------------------------
#' Tests and selects map
#'
#' Test and select map with the spatial expression corresponding to the time
#' frame selected.
#'
#' @param lst A list containing sf object containing two columns:
#' \code{geometry} and \code{province} , with named slot :
#' \code{XX_YEAR_YEAR_QUALITY}, XX is the country name in two letters code.
#' @param test_lst  A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time.
#'
#' @keywords internal
#' @noRd
sel_map <- function(lst, test_lst) {

  test <- lapply(lst, as.data.frame)
  test <- lapply(test, function(x) x[-which(names(x) == "geometry")])
  test <- test[!grepl("country", names(test))]
  test <- lapply(test, function(x)
    dictionary::match_pattern(x, "province", test_lst))
  names(test) <- gsub("^.._|_.{3,4}$", "", names(test))

  sel1 <- names(test[which(substr(test, 1, 4) != substr(names(test), 1, 4))])
  sel2 <- names(test[which(substr(test, 6, 9) != substr(names(test), 6, 9))])
  sel <- intersect(sel1, sel2)
  if (length(sel) != 0) {
    total_lst <- lst[!grepl(paste(sel, collapse = "|"), names(lst))]
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
#' @details The functions  needs a named vector, \code{hash} and \code{d.hash}
#' arguments, to translate the \code{NAME_1} column (and \code{NAME_2} if
#' necessary) from GADM \url{https://gadm.org} in a standardized English
#' version. We advice to use the named vector \code{xx_province} for admin1 or
#' \code{xx_district} for admin2
#' contained in the \code{dictionary}package, for example:
#' \code{\link[dictionary]{kh_province}}. If no \code{hash} and/or \code{d.hash}
#'  arguments is missing the column(s) \code{NAME_1} and/or \code{NAME_2} are
#' encoded in UNICODE and keep in native language.
#' \cr\cr
#' The function needs also a list of event (split/merge/rename/
#' complexe merge/complexe split) in a standardized format to recreate
#' historical map. We advice to use or the copy the format of the list
#' \code{xx_history} contained in the package \code{dictionary}.
#' For example: \code{\link[dictionary]{kh_history}}.
#' If no list are inputed in the \code{lst_history} argument, only the current
#' map of admin1 administrative boundary and the country boundary in high and
#' low resolution in a list will be created.
#' \cr\cr
#' The package \code{dictionary} is available on GitHub, to install it, it
#' necessary to have the \code{devtools} package:
#' \code{devtools::install_github("choisy/dictionary")}
#' \cr\cr
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on
#' each map object with the tolerance (argument \code{tolerance}) value in the
#' metric of the input object.
#' \cr\cr
#' The function uses the function \code{\link[sptools]{gadm}} from the package
#' \code{gadm}, to have more information on the parameters \code{save},
#' \code{path} and \code{intlib}, please take a look at the help of this
#' function.
#' \cr\cr
#' The arguments \code{lst_province_year} should be input as a list of charactor
#' vector containing the names of the admin1 written in a same way as
#' \code{hash} and/or \code{lst_history} ordered by year of change in
#' administrative boundaries.
#' We advice to use or the copy the format of the list \code{xx_province_year}
#' contained in the package \code{dictionary}. For example:
#' \code{\link[dictionary]{kh_province_year}}.
#' \cr\cr
#' The output of the function is a named list: the admin1 boundaries named are
#' named as: the 2 characters ISO code, the year of expression of this admin1
#' administrative boundaries and the resolution. For example:
#' "vn_1997_2004_high" for the admin1 boundaries of Vietnam from 1997-01-01
#' until 2004-01-01 (not include) in high quality.
#'
#' @param country character string, name of the country to download.
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See \code{Details} for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complexe merge/
#'  complexe split) and a slot year. See \code{Details} for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020".
#' @param d.hash used in case of \code{complexe split} or \code{complexe merge}
#' in the \code{lst_history} object.  named character vector containing the
#' translation in English (standardized version) of the admin2 names.
#' See \code{Details} for more information.
#' @param tolerance numeric for thinning (simplification). the tolerance value
#'  should be in the metric of the input object (cf. from function
#'  \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = 0.01.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a specific path or not. If \code{NULL}, it will be asked interactively.
#' By default \code{FALSE}.
#' @param path character string, path to save the downloaded file. If
#' \code{NULL}, the file will be saved in the working directory. By default
#' \code{NULL}.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' By default \code{TRUE}.
#' @param lst_province_year A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time. See \code{Details} for more inforamtion.
#' @param force boolean, force to download the file even if already in the path.
#' By default \code{FALSE}.
#'
#' @return a list of \code{sf} object containing the maps of admin1 a
#' dministrative boundaries and two maps of the country boundaries (one in high
#' resolution and one in low resolution).
#'
#' @examples
#' library(dictionary)
#'
#' kh_map <- hist_map("Cambodia", kh_province, kh_history)
#'
#' @importFrom sf st_crs st_bbox st_as_sf
#' @importFrom sptools sf_aggregate_lst
#' @importFrom stats setNames
#' @importFrom dictionary match_pattern
#'
#' @export
hist_map <- function(country, hash, lst_history, from = "1960",
                     to = "2020", d.hash = NULL, tolerance = 0.01,
                     save = FALSE, path = NULL, intlib = TRUE, force = FALSE,
                     lst_province_year = NULL) {

  if (missing(hash)) hash <- NULL
  if (missing(lst_history)) lst_history <- NULL

  # ACTUAL MAP
  df_sf <- current_map(country = country, hash = hash,
                       lst_history = lst_history, from = from, to = to,
                       d.hash = d.hash, save = save, path = path,
                       intlib = intlib, force = force)

  boundbox <- st_bbox(df_sf)
  crs <- st_crs(df_sf)

  # COUNTRY
  gadm0r <- gadm(country, "sf", 0, save = save, path = path,
                 intlib = intlib, force = force)
  gadm0r <- gadm0r[, -which(names(gadm0r) == "GID_0")] #%>%
  names(gadm0r)[which(names(gadm0r) == "NAME_0")] <- "country"
  gadm0r <- sf::st_as_sf(gadm0r)
  gadm0r <- define_bbox_proj(gadm0r, boundbox, crs)

  gadm0 <- thin_polygons(gadm0r, tolerance = tolerance)
  gadm0 <- define_bbox_proj(gadm0, boundbox, crs)

  # SELECT THE YEARS & MAKE THE LIST OF OLD MAP
  from <-  as.Date(paste0(from, "-01-01"))
  to <- as.Date(paste0(to, "-12-31"))
  if (is.null(lst_history)) {

    sel_year <- NULL
    df_sf_r <- thin_polygons(df_sf, tolerance = tolerance)
    df_sf_r <- define_bbox_proj(df_sf_r, boundbox, crs)
    total_lst <- list(setNames(list(df_sf, df_sf_r), c("high", "low")))
    total_lst <- setNames(total_lst, lubridate::year(Sys.time()))

  } else {

    sel_year <- lapply(lst_history, "[", "year")
    sel_year <- c(from, as.Date(unlist(sel_year)))
    sel_year <- unique(sel_year)
    sel_year <- sel_year[which(sel_year < to & sel_year >= from)]
    sel_year <- format(sel_year, "%Y")

    total_lst <- lapply(seq_along(sel_year), function (x) {

      if (country == "Vietnam" & sel_year[x] >= "2008") {
        old_mapr <- gadm(country, "sf", 1, save = save, path = path,
                         intlib = intlib, force = force)
        old_mapr <- transform(old_mapr,
                              province = translate(old_mapr$NAME_1, hash))
        old_mapr <- old_mapr[, c("province", "geometry")]
        old_mapr <- sf::st_as_sf(old_mapr)
      } else {
        old_mapr <- sf_aggregate_lst(df_sf, lst_history, from = sel_year[x])
        old_mapr <- define_bbox_proj(old_mapr, boundbox, crs)
      }
      old_map <- thin_polygons(old_mapr, tolerance = tolerance)
      old_map <- define_bbox_proj(old_map, boundbox, crs)
      setNames(list(old_mapr, old_map), c("high", "low"))
    })
    date_lst <- paste(sel_year,
                      c(sel_year[-1], as.numeric(format(to, "%Y")) + 1),
                      sep = "_")
    total_lst <- setNames(total_lst, date_lst)
  }
  # APPEND COUNTRY MAP
  country_lst <- setNames(list(country = gadm0r, gadm0), c("high", "low"))
  country_lst <- setNames(list(country_lst), "country")
  total_lst <- append(total_lst, country_lst)

  # MAKE NAME FILE
  name <- lapply(seq_along(total_lst), function(x) {
    names_lst <- names(total_lst[[names(total_lst)[x]]])
    cntry_code <- tolower(countrycode::countrycode(country, "country.name",
                                                     "iso2c"))
    names_lst <- paste0(cntry_code, "_", names(total_lst)[x], "_", names_lst)
  })
  names_lst <- unlist(name)
  total_lst <- setNames(unlist(total_lst, FALSE), names_lst)

  if (is.null(lst_province_year) == FALSE) {
    total_lst <- sel_map(total_lst, lst_province_year)
  }
 total_lst
}

## quiets concerns of R CMD check for the values that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "GID_0", "NAME_0",
                                                        "NAME_1", "NAME_2",
                                                        "district", "geometry",
                                                        "province"))
