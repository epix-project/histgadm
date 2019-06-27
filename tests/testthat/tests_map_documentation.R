library(dictionary) # for "XX_history", "XX_admin1"
library(usethis)

context("`map_documentation`")

testthat::test_that("`map_documentation` returns the correct output", {

  tmp <- file.path(tempdir(), "pkgtest")
  create_package(tmp, open = FALSE)
  map_data(tmp, "Cambodia", kh_admin1, kh_history)
  map_documentation(tmp)

  test1 <- dir(paste0(tmp, "/man/"))

  testthat::expect_length(test1, 8)
  unlink(tmp, recursive = TRUE)
})
