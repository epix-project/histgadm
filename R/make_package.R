#' Creates data and documentation for one country (internal)
#'
#' @param country character string, country name.
#' @param path character string, path of the package.
#' @param from Initial date of the time range selected, of the class Date,
#'  character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#' or numeric, by default "2020".
#' @param tolerance numeric for thinning (simplification), the tolerance value
#' should be in the metric of the input object (cf. from function
#' \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = NULL.
#' @param append_country boolean, append the country level in the
#' final list. by default, FALSE
#'
#' @keywords internal
#' @importFrom countrycode countrycode
#' @noRd
internal_data <- function(country, path, from = "1960", to = "2020",
                          append_country = FALSE, tolerance = NULL) {

  suppressWarnings(ccode <- countrycode::countrycode(country, "country.name",
                                                     "iso2c"))
  ccode <- tolower(ccode)
  if (is.na(ccode)) {
    stop(paste0("The `country` inputted:", country, " , was not matched.
                A complete description of available country languages is ",
                "available in the package `countrycode`:",
                " ?countrycode::codelist"))
  }
  province <- eval(parse(text = paste0("dictionary::", ccode, "_province")))
  hist <- eval(parse(text = paste0("dictionary::", ccode, "_history")))
  event_hist <- lapply(hist, "[", "event")
  if (any(grepl("complex", unlist(event_hist)))) {
    district <- eval(parse(text = paste0("dictionary::", ccode, "_district")))
  } else {
    district <- NULL
  }

  map_data(pckg_path = path, country = country, hash = province,
           lst_history = hist, from = from, to = to, d.hash = district,
           tolerance = tolerance, append_country = append_country)
  map_documentation(path)
}

# ------------------------------------------------------------------------------
#' Configures the intial files of the package
#'
#' Creates a package for the gadm data, allow you to download (if wanted) data
#' from gadm and recreates historical map from a time range (interactive input)
#'
#' @param path character string, path of the package.
#' @param name_pkg character string, name of the package.
#' @param tolerance numeric for thinning (simplification), the tolerance value
#' should be in the metric of the input object (cf. from function
#' \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = NULL.
#' @param append_country boolean, append the country level in the
#' final list. by default, FALSE
#'
#' @importFrom usethis create_package
#' @import dictionary
#'
#' @export
initial_pkg <-  function(path, name_pkg, tolerance = NULL,
                         append_country = FALSE) {

  pkg_path <-  paste0(path, "/", name_pkg)
  usethis::create_package(pkg_path, open = FALSE,
                          fields = list(Depends = "R (>= 2.10)",
                                        Imports = "sf"))

  message(cat(paste0("\n",
                     "Do you want to download GADM file from the internet?",
                     " y / n (default)")))
  ans <- readline("Selection: ")
  if (ans %in% c("y", "yes")) {

    message(cat(
      paste0("\n", "For which country do you want to download file?")))
    message(cat(
             "Currently, the function is working only for: ",
             "Cambodia, Laos, Thailand and Vietnam. For other country, use ",
             "the functions `map_data`, `map_documentation`. \n",
             "The country name should be input in full name and in English, \n",
             "For example: Cambodia \n",
             "Multiple country name is also accepted, separated by a ','. \n",
             "For example: Vietnam, Cambodia \n"))
    ans <- readline("Selection: ")
    if (grepl(",", ans)) {
      ans <- strsplit(ans, ",")
      ans <- unlist(lapply(ans, trimws))
    }

    message(cat(
      paste0("\n", "For which time range do you want to download file?")))
    message(cat(
             "The time range should be input in date format separateb by '-',",
             " by default the time range is '1960-01-01 2020-12-31'. \n",
             "For example: 1960-01-01 2020-12-31"))
    ans_date <- readline("Selection: ")
    if (ans_date == "") {
      from <- "1960-01-01"; to <- "2020-12-31"
    } else {
      date_lst <-  strsplit(ans_date, " ")
      from <- unlist(lapply(date_lst, "[", 1))
      to <- unlist(lapply(date_lst, "[", 2))
    }

    if (length(ans) > 1) {
      lapply(ans, function(x) {
        internal_data(x, pkg_path, from = from, to = to,
                      append_country = append_country, tolerance = tolerance)
        })
    } else
      internal_data(ans, pkg_path, from = from, to = to,
                    append_country = append_country, tolerance = tolerance)
  }
}
