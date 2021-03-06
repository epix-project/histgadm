library(dictionary) # for "XX_history", "XX_admin1"
library(usethis)

context("`map_data`")

test_that("`map_data` returns the correct output", {

  tmp <- file.path(tempdir(), "pkgtest")
  usethis::create_package(tmp, open = FALSE)
  map_data(tmp, "Cambodia", kh_admin1, kh_history)

  test1 <- dir(paste0(tmp, "/data/"))
  testthat::expect_length(test1, 3)

  map_data(tmp, "Cambodia", kh_admin1, kh_history, save = NULL)

  test1 <- dir(paste0(tmp, "/data/"))
  testthat::expect_length(test1, 3)

  unlink(tmp, recursive = TRUE)

})
