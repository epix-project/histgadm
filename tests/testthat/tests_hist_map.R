library(dictionary) # for "match_pattern", "XX_history", "XX_admin1",
# "XX_admin2"

context("`hist_map`")

test_that("`hist_map` returns the correct output", {

  match_yp <- function(lst, test_lst) {
    test <- lapply(lst, "[[", "admin1")
    test <- test[!unlist(lapply(test, is.null))]
    test <- lapply(test, function(x) data.frame(prov = x))
    test <- lapply(test, function(x) match_pattern(x, "prov", test_lst))
    test <- unique(unlist(test))
    test
  }

  test1a <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE)
  testthat::expect_equal(length(test1a), 3)

  test1b <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE, lst_admin1_year = kh_admin1_year)
  testthat::expect_equal(length(test1b), 3)

  test1c <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE, tolerance = 0.05)
  testthat::expect_equal(length(test1c), 3)

  test1d <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                               save = FALSE, append_country = TRUE)
  testthat::expect_equal(length(test1d), 4)

  test1e <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE, append_country = TRUE, tolerance = 0.05)
  testthat::expect_equal(length(test1e), 4)

  test2a <- hist_map("Laos", la_admin1, la_history, d.hash = la_admin2,
                    save = FALSE, intlib = FALSE)
  testthat::expect_equal(length(test2a), 3)

  test2b <- histgadm:::current_map("Vietnam", la_admin1, la_history,
                                   d.hash = la_admin2, from = 1960, to = 2020,
                                   save = FALSE, path = NULL, intlib = FALSE,
                                   force = FALSE)
  testthat::expect_equal(names(test2b), c("admin1", "admin2", "geometry"))

  vn_08 <- hist_map("Vietnam", vn_admin1, vn_history, save = FALSE,
                     from = 2008, to = 2008, intlib = FALSE)
  test3 <- match_yp(vn_08, vn_admin1_year)
  testthat::expect_equal(test3, "2008-2020")

  vn_8082 <- hist_map("Vietnam", vn_admin1, vn_history, save = FALSE,
                       from = 1980, to = 1982, intlib = FALSE)
  test4 <- match_yp(vn_8082, vn_admin1_year)
  testthat::expect_equal(test4, "1979-1990")

  test5 <- hist_map("France", intlib = FALSE, save = FALSE)
  testthat::expect_equal(length(test5), 1)

  test6 <- hist_map("Thailand", th_admin1, th_history, intlib = FALSE,
                    lst_admin1_year = th_admin1_year, from = "1960",
                    to = "1980", save = FALSE)
  testthat::expect_equal(length(test6), 2)
})
