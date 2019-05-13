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
#'  after, a slot before, a slotevent (split/merge/rename/ complex merge/
#'  complex split) and a slot year. See \code{Details} for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param d.hash used in case of \code{complex split} or \code{complex merge}
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
    name_history <- lapply(event_history, names)
    event_history <- lapply(event_history, "[[", "event")
  }

  # exception for Vietnam
  if (country == "Vietnam" &
      as.Date(paste0(from, "-01-01")) < as.Date("2008-01-01")) {
    if (any(grepl("complex|merge", event_history)) &
        any(grepl("d.bef", name_history))) {
      df_sf <- gadm(country, "sf", 2, save = save, path = path,
                    intlib = intlib, force = force)
      df_sf <- transform(df_sf, province = translate(df_sf$NAME_1, hash),
                         district = translate(df_sf$NAME_2, d.hash))
      df_sf <- df_sf[, c("province", "district", "geometry")]
    } else {
      df_sf <- get("vn_a1_0407")
      df_sf <- transform(df_sf, province = translate(df_sf$NAME_2, hash))
      df_sf <- df_sf[, c("province", "geometry")]
    }
  } else if (!is.null(lst_history) &&
             any(grepl("complex|merge", event_history)) &&
             any(grepl("d.bef", name_history))) {
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
#' Download  Country Administrative Boundaries
#'
#' Download country boundaries
#'
#' @param country character string, name of the country to download.
#' @param boundbox character, bounding box.
#' @param crs character, coordinate reference system.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a specific path or not. If \code{NULL}, it will be asked interactively.
#' By default \code{FALSE}.
#' @param path character string, path to save the downloaded file. If
#' \code{NULL}, the file will be saved in the working directory. By default
#' \code{NULL}.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' By default \code{TRUE}.
#' @param force boolean, force to download the file even if already in the path.
#' By default \code{FALSE}.
#' @param tolerance numeric for thinning (simplification). the tolerance value
#'  should be in the metric of the input object (cf. from function
#'  \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = NULL.
#'
#' @importFrom sptools gadm define_bbox_proj thin_polygons
#' @importFrom sf st_as_sf
#'
#' @keywords internal
#' @noRd
download_country <- function(country, boundbox, crs, save, path, intlib,
                             force, tolerance) {
  gadm0 <- gadm(country, "sf", 0, save = save, path = path,
                intlib = intlib, force = force)
  gadm0 <- gadm0[, -which(names(gadm0) == "GID_0")]
  names(gadm0)[which(names(gadm0) == "NAME_0")] <- "country"
  gadm0 <- sf::st_as_sf(gadm0)
  gadm0 <- sptools::define_bbox_proj(gadm0, boundbox, crs)

  if (!is.null(tolerance)) {
    gadm0 <- sptools::thin_polygons(gadm0, tolerance = tolerance)
    gadm0 <- sptools::define_bbox_proj(gadm0, boundbox, crs)
  }
  gadm0
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
  names(test) <- gsub("^.._", "", names(test))

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
#' admin1).
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
#' complex merge/complex split) in a standardized format to recreate
#' historical map. We advice to use or the copy the format of the list
#' \code{xx_history} contained in the package \code{dictionary}.
#' For example: \code{\link[dictionary]{kh_history}}. Example of a list with
#' complex events:  \code{\link[dictionary]{la_history}}
#' If no list are inputed in the \code{lst_history} argument, only the current
#' map of admin1 administrative boundary in a list will be created.
#' \cr\cr
#' The package \code{dictionary} is available on GitHub, to install it, it
#' necessary to have the \code{devtools} package:
#' \code{devtools::install_github("choisy/dictionary")}
#' \cr\cr
#' The function can perform \code{\link[maptools]{thinnedSpatialPoly}} on
#' each map object with the tolerance (argument \code{tolerance}) value in the
#' metric of the input object (optionnal argument). By default, the argument is
#' set to NULL and makes not simplification.
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
#' administrative boundaries. For example:
#' "vn_1997_2004" for the admin1 boundaries of Vietnam from 1997-01-01
#' until 2004-01-01 (not include).
#'
#' @param country character string, name of the country to download.
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See \code{Details} for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/ complex merge/
#'  complex split) and a slot year. See \code{Details} for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020".
#' @param d.hash used in case of \code{complex split} or \code{complex merge}
#' in the \code{lst_history} object.  named character vector containing the
#' translation in English (standardized version) of the admin2 names.
#' See \code{Details} for more information.
#' @param tolerance numeric for thinning (simplification), the tolerance value
#'  should be in the metric of the input object (cf. from function
#'  \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = NULL.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a specific path or not. If \code{NULL}, it will be asked interactively.
#' By default \code{FALSE}.
#' @param path character string, path to save the downloaded file. If
#' \code{NULL}, the file will be saved in the working directory. By default
#' \code{NULL}.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' By default \code{TRUE}.
#' @param force boolean, force to download the file even if already in the path.
#' By default \code{FALSE}.
#' @param lst_province_year A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time. See \code{Details} for more information.
#' @param append_country boolean, append the country level in the
#' final list. by default, FALSE
#'
#' @return a list of \code{sf} object containing the maps of admin1 a
#' dministrative boundaries.
#'
#' @examples
#' library(dictionary)
#'
#' kh_map <- hist_map("Cambodia", kh_province, kh_history)
#'
#' @importFrom sf st_crs st_bbox st_as_sf
#' @importFrom sptools sf_aggregate_lst thin_polygons define_bbox_proj
#' @importFrom stats setNames
#' @importFrom dictionary match_pattern translate
#'
#' @export
hist_map <- function(country, hash, lst_history, from = "1960",
                     to = "2020", d.hash = NULL, tolerance = NULL,
                     save = FALSE, path = NULL, intlib = TRUE, force = FALSE,
                     lst_province_year = NULL, append_country = FALSE) {

  if (missing(hash)) hash <- NULL
  if (missing(lst_history)) lst_history <- NULL

  # ACTUAL MAP
  df_sf <- current_map(country = country, hash = hash,
                       lst_history = lst_history, from = from, to = to,
                       d.hash = d.hash, save = save, path = path,
                       intlib = intlib, force = force)
  boundbox <- st_bbox(df_sf)
  crs <- st_crs(df_sf)

  if (!is.null(tolerance)) {
    df_sf <- sptools::thin_polygons(df_sf, tolerance = tolerance)
    df_sf <- sptools::define_bbox_proj(df_sf, boundbox, crs)
  }

  # SELECT THE YEARS & MAKE THE LIST OF OLD MAP
  from <-  as.Date(paste0(from, "-01-01"))
  to <- as.Date(paste0(to, "-12-31"))
  if (is.null(lst_history)) {

    sel_year <- NULL
    total_lst <- setNames(list(df_sf), format(Sys.time(), "%Y"))

  } else {

    sel_year <- lapply(lst_history, "[", "year")
    sel_year <- c(from, as.Date(unlist(sel_year)))
    sel_year <- unique(sel_year)
    sel_year <- sel_year[which(sel_year < to & sel_year >= from)]
    sel_year <- format(sel_year, "%Y")

    total_lst <- lapply(seq_along(sel_year), function (x) {

      if (country == "Vietnam" & sel_year[x] >= "2008") {
        old_map <- gadm(country, "sf", 1, save = save, path = path,
                        intlib = intlib, force = force)
        old_map <- transform(old_map,
                             province = translate(old_map$NAME_1, hash))
        old_map <- old_map[, c("province", "geometry")]
        old_map <- sf::st_as_sf(old_map)
      } else {
        old_map <- sf_aggregate_lst(df_sf, lst_history, from = sel_year[x])
        old_map <- sptools::define_bbox_proj(old_map, boundbox, crs)
      }

     # if (!is.null(tolerance)) {
      #  old_map <- sptools::thin_polygons(old_map, tolerance = tolerance)
      #  old_map <- sptools::define_bbox_proj(old_map, boundbox, crs)
      #}

      old_map <- old_map[, c("province", "geometry")]
      old_map <- transform(old_map, province = as.character(old_map$province))
      old_map <- sf::st_as_sf(old_map)

    })
    date_lst <- paste(sel_year,
                      c(sel_year[-1], as.numeric(format(to, "%Y")) + 1),
                      sep = "_")
    total_lst <- setNames(total_lst, date_lst)
  }

  # APPEND COUNTRY MAP
  if (isTRUE(append_country)) {
    gadm0 <- download_country(country = country, boundbox = boundbox, crs = crs,
                              save = save, path = path, intlib = intlib,
                              force = force, tolerance = tolerance)
    country_lst <- setNames(list(gadm0), "country")
    total_lst <- append(total_lst, country_lst)
  }

  # MAKE NAME FILE
  name <- lapply(seq_along(total_lst), function(x) {
    cntry_code <- tolower(countrycode::countrycode(country, "country.name",
                                                     "iso2c"))
    paste0(cntry_code, "_", names(total_lst)[x])
  })
  names_lst <- unlist(name)
  total_lst <- setNames(total_lst, names_lst)

  if (is.null(lst_province_year) == FALSE) {
    total_lst <- sel_map(total_lst, lst_province_year)
  }
 total_lst
}
