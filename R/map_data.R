# ------------------------------------------------------------------------------
#' Creates a list of historical map in a package
#'
#' Creates a list of historical map with the function
#' \code{\link[histgadm]{hist_map}} and if \code{save} is \code{TRUE}, stores
#' the download GADM (\url{https://gadm.org}) files in a folder \code{data_raw}
#' and saves the output maps in the folder \code{data} of a package.
#'
#' Please find more details on the arguments of the function on the
#' documentation of the function \code{\link[histgadm]{hist_map}}.
#'
#' @param pckg_path character string path of the package.
#' @param country character string, name of the country to download.
#' @param hash named character vector containing the translation in English
#'  (standardized version) of the admin1 names. See \code{Details} for more
#'  information.
#' @param lst_history A list containing a list of event, each code with a slot
#'  after, a slot before, a slotevent (split/merge/rename/complex merge/
#'  complex split) and a slot year. See \code{Details} for more information.
#' @param from Initial date of the time range selected, of the class Date,
#'   character or numeric. By default "1960".
#' @param to Final date of the time range selected, of the class Date, character
#'  or numeric, by default "2020".
#' @param d.hash used in case of \code{complex split} or \code{complex merge}
#' in the \code{lst_history} object.  named character vector containing the
#' translation in English (standardized version) of the admin2 names. See
#' \code{Details} for more information.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' By default \code{TRUE}.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a data-raw folder path or not. By default \code{TRUE}.
#' If \code{NULL}, it will be asked interactively.
#' @param force boolean, force to download the file even if already in the path.
#' By default \code{FALSE}.
#' @param tolerance numeric for thinning (simplification). the tolerance value
#' should be in the metric of the input object (cf. from function
#' \code{\link[maptools]{thinnedSpatialPoly}}). By default, tolerance = NULL.
#' @param lst_admin1_year A list containing the spatial expression of admin1
#' for each year of change, use to select the map expressed with the right
#' admin1 definition in time. See \code{Details} for more information.
#' @param append_country boolean, append the country level in the
#' final list. by default, FALSE
#'
#' @examples
#' library(dictionary)
#' \dontrun{
#' map_data("PACKAGE", "Cambodia", kh_admin1, kh_history)
#' }
#' @export
map_data <- function(pckg_path, country, hash, lst_history, from = "1960",
                     to = "2020", d.hash = NULL, intlib = FALSE, save = TRUE,
                     force = FALSE, tolerance = NULL, lst_admin1_year = NULL,
                     append_country = FALSE) {
  path0 <- getwd()
  setwd(pckg_path)
  datarawdir <- paste0(pckg_path, "/data-raw")
  if (!dir.exists(datarawdir)) dir.create(datarawdir)
  datadir <- paste0(pckg_path, "/data")
  if (!dir.exists(datadir)) dir.create(datadir)

  if (is.null(save)) {
    message(cat(
      "\n Do you want to save the map(s) from GADM in the data-raw folder ?",
                " (yes (default)/ no) \n"))
    ans <- readline("Selection: ")
    if (ans %in% c("yes", "y", "Y", "")) {
      save <- TRUE
    }
    if (ans %in% c("no", "N", "n")) {
      save <- FALSE #nocov
    }
  }

  data <- hist_map(country = country, hash = hash, lst_history = lst_history,
                   from = from, to = to, d.hash = d.hash,
                   tolerance = tolerance, intlib = intlib, save = save,
                   path = datarawdir, lst_admin1_year = lst_admin1_year,
                   append_country = append_country)
  list2env(data, envir = environment())
  eval(parse(text = paste0("usethis::use_data(`", paste(names(data),
                                                  collapse = "`, `"),
                     "`, overwrite = TRUE)")))
  setwd(path0)
}
