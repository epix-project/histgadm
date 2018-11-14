# ------------------------------------------------------------------------------
#' Format documentation of a data.frame
#'
#' From a data frame, return one vector of length one containing:
#' An object of class \code{data.frame, sf} with `THE DIMENSION OF THE DF`.
#' And a list of the columns name and the class of each column.
#'
#' @param df a object of class `data.frame`.
#'
#' @importFrom roxygen2 object_format
#' @keywords internal
#' @noRd
make_format <- function(df){
  paste0(roxygen2::object_format(df), "\n \\itemize{",
         lapply(seq_len(dim(df)[2]), function(x) {
           paste0("\\item \\code{", names(df)[x], "} ",
                  paste0("A column of class : ",
                         class(df[[x]]) %>% paste(collapse = ", "), "."))
         }) %>%
           paste0(collapse = "\n"), "}")
}

# ------------------------------------------------------------------------------
#' Makes documentation for sf map in a package
#'
#' Create a Rd file for the data, containing the documentation for each sf
#' object in the package.
#'
#' @param path character string path of the package.
#'
#' @importFrom stringr str_extract
#' @importFrom Rd2roxygen create_roxygen
#' @importFrom utils capture.output
#' @export
map_documentation <- function(path) {

  list_tab <- dir(paste0(path, "/data/"))
  tot_rd <-  lapply(seq_along(list_tab), function(x){

    load(paste0(path, "/data/", list_tab[x]))
    df <- get(gsub(".rda", "", list_tab[x]))
    from <- list_tab[x] %>% gsub("[^[:digit:]]", "", .)  %>% substr(1, 4)
    to <- list_tab[x] %>% gsub("[^[:digit:]]", "", .)  %>% substr(5, 8)
    quality <- list_tab[x] %>% stringr::str_extract("high|low")
    country <- list_tab[x] %>% substr(1, 2) %>%
      countrycode::countrycode("iso2c", "country.name")
    source <- paste0("GADM (version ",
                     dir(paste0(path, "/data-raw/")) %>% substr(5, 6) %>%
                       unique %>% strsplit("") %>% unlist %>%
                       paste(collapse = "."),
                     ") data base from \\url{www.gadm.org}")

    if (grep("[[:digit:]]", list_tab[x]) %>% length > 0) {

      doc <- list(
        title = paste0("Admin1 Administrative boundaries of ", country,
                       " from ", from, " to ", to, "."),
        format = make_format(df),
        desc = paste0(
          "Maps of the admin1 administrative boundaries of ", country,
          " expressed from ", from, " to ", to, " in ", quality, " quality."),
        source = source)

    } else {

      doc <- list(
        title = paste0(country, "Country boundaries"),
        format = make_format(df),
        desc = paste0(
          "Maps of the country administrative boundaries of ", country,
          " expressed in ", quality, "quality."),
        source = source)
    }
    doc <- capture.output(cat(Rd2roxygen::create_roxygen(doc), sep = "\n"))
    doc <- c(doc,
             paste0("'", gsub(".rda", "", list_tab[x]) %>%
                      as.character(), "'\n"))
  })
  writeLines(unlist(tot_rd), con = paste0(path, "/R/data.R"), sep = "\n")
  roxygen2::roxygenize(path)
}
