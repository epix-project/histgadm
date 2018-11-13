library(dictionary) # for "XX_history", "XX_province"

context("`map_data`")

test_that("`map_data` returns the correct output", {

  setwd("~/OneDrive/Shared/histgadm")
  path <- "~/OneDrive/Shared/histgadm/tests/testthat/pkgtest"
  map_data(path, "Cambodia", kh_province, kh_history)
  test1 <- dir("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/data/")

  testthat::expect_length(test1, 8)
  unlink("tests/testthat/pkgtest/data/", recursive = TRUE)
  unlink("tests/testthat/pkgtest/data-raw/", recursive = TRUE)

})
