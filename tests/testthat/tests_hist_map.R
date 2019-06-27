library(magrittr)  # for "%>%"
library(purrr) # for "map", "discard"
library(dictionary) # for "match_pattern", "XX_history", "XX_admin1",
# "XX_admin2"

context("`hist_map`")

test_that("`hist_map` returns the correct output", {

  test1a <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE)
  testthat::expect_equal(length(test1a), 8)

  test1b <- hist_map("Cambodia", kh_admin1, kh_history, intlib = FALSE,
                     save = FALSE, lst_admin1_year = kh_admin1_year)
  testthat::expect_equal(length(test1b), 8)

  test2 <- hist_map("Laos", la_admin1, la_history, d.hash = la_admin2,
                    save = FALSE, intlib = FALSE)
  testthat::expect_equal(length(test2), 8)

  vn_08 <- hist_map("Vietnam", vn_admin1, vn_history, save = FALSE,
                     from = 2008, to = 2008, intlib = FALSE)
  test3 <- vn_08 %>% purrr::map("admin1") %>% purrr::discard(is.null) %>%
    purrr::map(data.frame) %>%
    purrr::map(dictionary::match_pattern, ".x..i..", vn_admin1_year) %>%
    unlist() %>% unique()
  testthat::expect_equal(test3, "2008-2020")

  vn_8082 <- hist_map("Vietnam", vn_admin1, vn_history, save = FALSE,
                       from = 1980, to = 1982, intlib = FALSE)
  test4 <- vn_8082 %>%
    purrr::map("admin1") %>%
    purrr::discard(is.null) %>%
    purrr::map(data.frame) %>%
    purrr::map(dictionary::match_pattern, ".x..i..", vn_admin1_year) %>%
    unlist() %>% unique()
  testthat::expect_equal(test4, "1979-1990")

  test5 <- hist_map("France", intlib = FALSE, save = FALSE)
  testthat::expect_equal(length(test5), 4)

  test6 <- hist_map("Thailand", th_admin1, th_history, intlib = FALSE,
                    lst_admin1_year = th_admin1_year, from = "1960",
                    to = "1980", save = FALSE)
  testthat::expect_equal(length(test6), 6)

})
