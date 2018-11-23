library(dictionary) # for "XX_history", "XX_province"
library(usethis)

context("`make_package`")

test_that("`initial_pkg` returns the correct output", {

  tmp <- file.path(tempdir(), "pkgtest")
  dir.create(tmp)
  initial_pkg(tmp, "test")

  test1 <- dir(paste0(tmp, "/test"))

  testthat::expect_length(test1, 4)
  unlink(tmp, recursive = TRUE)

  tmp <- file.path(tempdir(), "pkgtest")
  create_package(tmp, open = FALSE)
  histgadm:::internal_data("Laos", tmp)

  test2 <- dir(paste0(tmp,"/man/"))
  testthat::expect_length(test2, 8)
  unlink(tmp, recursive = TRUE)

  tmp <- file.path(tempdir(), "pkgtest")
  create_package(tmp, open = FALSE)
  histgadm:::internal_data("Cambodia", tmp)

  test3 <- dir(paste0(tmp,"/man/"))
  testthat::expect_length(test3, 8)
  unlink(tmp, recursive = TRUE)

})