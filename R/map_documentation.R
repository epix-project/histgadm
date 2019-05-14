# ------------------------------------------------------------------------------
#' Extract information from data
#'
#' This function is called to generate the information in the data
#'
#' @param x string character name
#' @keywords internal
#' @noRd
extract_info <- function(x, path){

  date <- gsub("[^[:digit:]]", "", x)
  from <- substr(date, 1, 4)
  to <- substr(date, 5, 8)

  country_iso <- substr(x, 1, 2)
  country <- countrycode::countrycode(country_iso, "iso2c", "country.name")

  version <- substr(dir(paste0(path, "/data-raw/")), 5, 6)
  version <- strsplit(unique(version), "")
  if (length(version) > 0) {
    source <- paste0("GADM (version ", paste(unlist(version), collapse = "."),
                     ")")
  } else {
    source <- "GADM"
  }
  source <-  paste0(source, " data base from \\url{www.gadm.org}")

  res <- c("from" = from, "to" = to, "source" = source, "country" = country)
  res
}

# ------------------------------------------------------------------------------
#' Format of an object
#'
#' This function is called to generate the default "Format" and returns the
#' class and dimension information.
#'
#' From (https://github.com/klutometis/roxygen/blob/master/R/object-format.R)
#'
#' @param x A data object
#' @return A `character` value with valid `Rd` syntax, or `NULL`.
#' @keywords internal
#' @noRd
obj_format <- function(x) {
  classes <- paste0("\\code{", class(x), "}")
  format <- paste0("with ", nrow(x), " rows and ", ncol(x), " columns")
  out <- paste0("An object of class ", classes, " ", format, ".")
  out
}


# ------------------------------------------------------------------------------
#' Format documentation of a data.frame
#'
#' From a data frame, return one vector of length one containing:
#' An object of class \code{data.frame, sf} with \code{THE DIMENSION OF THE DF}.
#' And a list of the columns name and the class of each column.
#'
#' @param df a object of class \code{data.frame}.
#'
#' @keywords internal
#' @noRd
make_format <- function(df) {
  paste0(obj_format(df), "\n \\itemize{",
         paste0(lapply(seq_len(dim(df)[2]), function(x) {
           paste0("\\item \\code{", names(df)[x], "} ",
                  paste0("A column of class : ",
                         paste(class(df[[x]]), collapse = ", "), "."))
         }),
         collapse = "\n"), "}")
}

# ------------------------------------------------------------------------------
#' Makes documentation for sf map in a package
#'
#' Create a Rd file for the data, containing the documentation for each sf
#' object in the package.
#'
#' @param path character string path of the package.
#'
#' @importFrom Rd2roxygen create_roxygen
#' @importFrom roxygen2 roxygenize
#' @importFrom utils capture.output
#' @export
map_documentation <- function(path) {

  list_tab <- grep("^.._|.rda$", dir(paste0(path, "/data/")), value = TRUE)

  tot_rd <- lapply(list_tab, function(x) {

    load(paste0(path, "/data/", x))
    df <- get(gsub(".rda", "", x))
    info <- extract_info(x, path)

    if (length(grep("[[:digit:]]", x)) > 0) {

      doc <- list(
        title = paste0("Admin1 Administrative boundaries of ", info["country"],
                       " from ", info["from"], " to ", info["to"], "."),
        format = make_format(df),
        desc = paste0(
          "Maps of the admin1 administrative boundaries of ", info["country"],
          " expressed from ", info["from"], " to ", info["to"], "."),
        source = info["source"])

    } else {

      doc <- list(
        title = paste0(info["country"], " country boundaries"),
        format = make_format(df),
        desc = paste0(
          "Maps of the country administrative boundaries of ", info["country"],
          "."),
        source = info["source"])
    }
    doc <- capture.output(cat(Rd2roxygen::create_roxygen(doc), sep = "\n"))
    doc <- c(doc, paste0("'", as.character(gsub(".rda", "", x)), "'\n"))
  })
  writeLines(unlist(tot_rd), con = paste0(path, "/R/data.R"), sep = "\n")
  roxygen2::roxygenize(path)
}
