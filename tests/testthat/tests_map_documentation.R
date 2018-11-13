library(dictionary) # for "XX_history", "XX_province"

context("`map_documentation`")

test_that("`map_documentation` returns the correct output", {

  setwd("~/OneDrive/Shared/histgadm")
  path <- "~/OneDrive/Shared/histgadm/tests/testthat/pkgtest"
  map_data(path, "Cambodia", kh_province, kh_history)
  map_documentation(path)

  test1 <- dir("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/man/")

  testthat::expect_length(test1, 8)
  unlink("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/data/",
         recursive = TRUE)
  unlink("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/data-raw/",
         recursive = TRUE)
  file.remove(dir("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/man/"))
  file.remove(dir("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/R/"))

})
