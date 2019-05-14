library(dictionary) # for "match_pattern", "XX_history", "XX_province",
# "XX_district"

context("`hist_map`")

test_that("`hist_map` returns the correct output", {

  match_yp <- function(lst, test_lst) {
    test <- lapply(lst, "[[", "province")
    test <- test[!unlist(lapply(test, is.null))]
    test <- lapply(test, function(x) data.frame(prov = x))
    test <- lapply(test, function(x) match_pattern(x, "prov", test_lst))
    test <- unique(unlist(test))
    test
  }

  test1a <- hist_map("Cambodia", kh_province, kh_history, intlib = FALSE,
                     save = FALSE)
  testthat::expect_equal(length(test1a), 3)

  test1b <- hist_map("Cambodia", kh_province, kh_history, intlib = FALSE,
                     save = FALSE, lst_province_year = kh_province_year)
  testthat::expect_equal(length(test1b), 3)

  test1c <- hist_map("Cambodia", kh_province, kh_history, intlib = FALSE,
                     save = FALSE, tolerance = 0.05)
  testthat::expect_lt(sf::st_bbox(test1a$kh_1960_1997[1, ])[[1]],
                      sf::st_bbox(test1c$kh_1960_1997[1, ])[[1]])

  test1d <- hist_map("Cambodia", kh_province, kh_history, intlib = FALSE,
                               save = FALSE, append_country = TRUE)
  testthat::expect_equal(length(test1d), 4)

  test1e <- hist_map("Cambodia", kh_province, kh_history, intlib = FALSE,
                     save = FALSE, append_country = TRUE, tolerance = 0.05)
  testthat::expect_lt(sf::st_bbox(test1d$kh_1960_1997[1, ])[[1]],
                      sf::st_bbox(test1e$kh_1960_1997[1, ])[[1]])

  test2a <- hist_map("Laos", la_province, la_history, d.hash = la_district,
                    save = FALSE, intlib = FALSE)
  testthat::expect_equal(length(test2a), 3)

  test2b <- histgadm:::current_map("Vietnam", la_province, la_history,
                                   d.hash = la_district, from = 1960, to = 2020,
                                   save = FALSE, path = NULL, intlib = FALSE,
                                   force = FALSE)
  testthat::expect_equal(names(test2b), c("province", "district", "geometry"))

  vn_08 <- hist_map("Vietnam", vn_province, vn_history, save = FALSE,
                     from = 2008, to = 2008, intlib = FALSE)
  test3 <- match_yp(vn_08, vn_province_year)
  testthat::expect_equal(test3, "2008-2020")

  vn_8082 <- hist_map("Vietnam", vn_province, vn_history, save = FALSE,
                       from = 1980, to = 1982, intlib = FALSE)
  test4 <- match_yp(vn_8082, vn_province_year)
  testthat::expect_equal(test4, "1979-1990")

  test5 <- hist_map("France", intlib = FALSE, save = FALSE)
  testthat::expect_equal(length(test5), 1)

  test6 <- hist_map("Thailand", th_province, th_history, intlib = FALSE,
                    lst_province_year = th_province_year, from = "1960",
                    to = "1980", save = FALSE)
  testthat::expect_equal(length(test6), 2)
})
