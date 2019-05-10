library(dictionary) # for "XX_history", "XX_province"
library(usethis)

context("`map_data`")

test_that("`map_data` returns the correct output", {

  tmp <- file.path(tempdir(), "pkgtest")
  create_package(tmp, open = FALSE)
  map_data(tmp, "Cambodia", kh_province, kh_history)

  test1 <- dir(paste0(tmp, "/data/"))

  testthat::expect_length(test1, 3)
  unlink(tmp, recursive = TRUE)

})
