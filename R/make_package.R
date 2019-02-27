#' Creates data and documentation for one country (internal)
#'
#' @param country character string, country name.
#' @param path character string, path of the package.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020".
#'
#' @keywords internal
#' @noRd
internal_data <- function(country, path, from = "1960", to = "2020") {

  suppressWarnings(
    ccode <- countrycode::countrycode(country, "country.name", "iso2c") %>%
    tolower())
  if (is.na(ccode)) {
    stop(paste0("The `country` inputed:", country, " , was not matched.
                A complete description of available country languages is ",
                "available in the package `countrycode`:",
                " ?countrycode::codelist"))
  }

  prov <- eply::evals(paste0("dictionary::", ccode, "_province"))
  province <- as.vector(prov) %>% setNames(attr(prov, "dimnames")[[1]])
  hist <- eply::evals(paste0("dictionary::", ccode, "_history"))
  if (hist %>% map("event") %>% grepl("complexe", .) %>% any) {
    distr <- eply::evals(paste0("dictionary::", ccode, "_district"))
    district <- as.vector(distr) %>% setNames(attr(distr, "dimnames")[[1]])
  } else {
    district <- NULL
  }

  map_data(path = path, country = country, hash = province, lst_history = hist,
           from = from, to = to, d.hash = district)
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
#'
#' @importFrom usethis create_package use_package use_description
#' @importFrom countrycode countrycode
#' @importFrom utils getAnywhere
#' @importFrom purrr map
#' @importFrom crayon silver
#' @import dictionary
#'
#' @export
initial_pkg <-  function(path, name_pkg) {

  pkg_path <-  paste0(path, "/", name_pkg)
  usethis::create_package(pkg_path, open = FALSE,
                          fields = list(Depends = "R (>= 2.10)"))
  usethis::use_package("sf")

  message(cat(paste0("\n",
                     "Do you want to download GADM file from the internet?",
                     " y / n (default)")))
  ans <- readline("Selection: ")
  if (ans %in% c("y", "yes")) {

    message(cat(
      paste0("\n", "For which country do you want to download file?")))
    message(cat(crayon::silver(paste0(
             "Currently, the function is working only for: ",
             "Cambodia, Laos, Thailand and Vietnam. For other country, use ",
             "the functions `map_data`, `map_documentation`. \n",
             "The country name should be input in full name and in English, \n",
             "For example: Cambodia \n",
             "Multiple country name is also accepted, separated by a ','. \n",
             "For example: Vietnam, Cambodia \n"))))
    ans <- readline("Selection: ")
    if (grepl(",", ans)) ans %<>% strsplit(",") %>% map(trimws) %>% unlist

    message(cat(
      paste0("\n", "For which time range do you want to download file?")))
    message(cat(crayon::silver(paste0(
             "The time range should be input in date format separateb by '-',",
             " by default the time range is '1960-01-01 2020-12-31'. \n",
             "For example: 1960-01-01 2020-12-31"))))
    ans_date <- readline("Selection: ")
    if (ans_date == "") {
      from <- "1960-01-01"; to <- "2020-12-31"
    } else {
      from <- ans_date %>% strsplit(" ") %>% purrr::map(1) %>% unlist
      to <- ans_date %>% strsplit(" ") %>% purrr::map(2) %>% unlist
    }

    if (length(ans) > 1) {
      lapply(ans, function(x) {
        internal_data(x, pkg_path, from = from, to = to)
        })
    } else
      internal_data(ans, pkg_path, from = from, to = to)
  }
}
