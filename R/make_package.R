#' Configures the intial files of the package
#'
#' Creates a package for the gadm data, allow you to download (if wanted) data
#' from gadm and recreates historical map from a time range (interactive input)
#'
#' @param path character string, path of the package.
#' @param name_pkg character string, name of the package
#'
#' @importFrom usethis create_package use_package
#' @importFrom countrycode countrycode
#' @importFrom utils getAnywhere
#' @import dictionary
#'
#' @export
initial_pkg <-  function(path, name_pkg) {

  pkg_path <-  paste0(path, "/", name_pkg)
  usethis::create_package(pkg_path, open = FALSE)
  #usethis::use_dev_package("R (>= 2.10)")
  usethis::use_package("sf")

  message("Do you want to download GADM file from the internet? y / n (default)"
          )
  ans <- readline()
  if (ans %in% c("y", "")) {
    message("For which country do you want to download file? /cr
            The country name should be input in full name and in English")
    ans <- readline()
    ccode <- countrycode::countrycode(ans, "country.name", "iso2c") %>%
      tolower()
    prov <- eply::evals(paste0("dictionary::", ccode, "_province"))
    province <- as.vector(prov) %>% setNames(attr(prov, "dimnames")[[1]])
    hist <- eply::evals(paste0("dictionary::", ccode, "_history"))
    map_data(path = pkg_path, country = ans, hash = province,
             lst_history = hist)
  }
}


# ------------------------------------------------------------------------------
#' Creates a list of historical map in a package
#'
#' Creates a list of historical map with the function
#' \code{\link[histgadm]{hist_map}} and stores the download GADM
#' (\url{https://gadm.org}) files in a folder data_raw and the output maps in
#' the folder data of a package.
#'
#' @param path character string path of the package.
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
#' @examples
#'
#' library(dictionary)
#' \dontrun{
#' map_data("PACKAGE", "Cambodia", kh_province, kh_history)
#' }
#'
#' @importFrom devtools use_data_raw
#' @importFrom eply evals
#' @importFrom usethis use_data
#'
#' @export
map_data <- function(path, country, hash, lst_history, from = "1960",
                     to = "2020", d.hash = NULL, tolerance = 0.01) {
  setwd(path)
  # data-raw
  datarawdir <- paste0(path, "/data-raw")
  if (!dir.exists(datarawdir)) usethis::use_data_raw()
  # data
  datadir <- paste0(path, "/data")
  if (!dir.exists(datadir)) dir.create(datadir)
  data <- hist_map(country = country, hash = hash, lst_history = lst_history,
                    from = from, to = to, d.hash = d.hash,
                    tolerance = tolerance, file_rm = FALSE, path = datarawdir)
  list2env(data, envir = environment())
  eply::evals(paste0("usethis::use_data(`", paste(names(data),
                                                  collapse = "`, `"),
                     "`, overwrite = TRUE)"))
}

