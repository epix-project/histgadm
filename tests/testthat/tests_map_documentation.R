
library(dictionary) # for "XX_history", "XX_province"

context("`map_documentation`")

testthat::test_that("`map_documentation` returns the correct output", {

  setwd("~/OneDrive/Shared/histgadm/tests/testthat")
  path <- "~/OneDrive/Shared/histgadm/tests/testthat/pkgtest"
  map_data(path, "Cambodia", kh_province, kh_history)
  setwd("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest")
  map_documentation(path)

  test1 <- dir("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/man/")

  testthat::expect_length(test1, 8)

  unlink("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/data/",
         recursive = TRUE)
  unlink("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/data-raw/",
         recursive = TRUE)
  file.remove(paste("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/man/",
                    dir("man/"), sep = ""))
  file.remove(paste("~/OneDrive/Shared/histgadm/tests/testthat/pkgtest/man/",
                    dir("R/"), sep = ""))

  setwd("~/OneDrive/Shared/histgadm")
})
