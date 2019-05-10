library(dictionary) # for "XX_history", "XX_province"
library(usethis)

context("`map_documentation`")

testthat::test_that("`map_documentation` returns the correct output", {

  tmp <- file.path(tempdir(), "pkgtest")
  usethis::create_package(tmp, open = FALSE)
  map_data(tmp, "Cambodia", kh_province, kh_history)
  map_documentation(tmp)

  test1 <- dir(paste0(tmp, "/man/"))

  testthat::expect_length(test1, 4)
  unlink(tmp, recursive = TRUE)
})
