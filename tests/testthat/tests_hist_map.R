library(magrittr)  # for "%>%"
library(purrr) # for "map", "discard"
library(dictionary) # for "match_pattern", "XX_history", "XX_province",
# "XX_district"

context("`hist_gadm`")

test_that("`hist_gadm` returns the correct output", {

  test1 <- hist_map("Cambodia", kh_province, kh_history)
  testthat::expect_equal(length(test1), 8)

  test2 <- hist_map("Laos", la_province, la_history, d.hash = la_district)
  testthat::expect_equal(length(test2), 8)

  vn_08 <- hist_map("Vietnam", vn_province, vn_history,
                     from = 2008, to = 2008)
  test3 <- vn_08 %>% purrr::map("province") %>% purrr::discard(is.null) %>%
    purrr::map(data.frame) %>%
    purrr::map(dictionary::match_pattern, ".x..i..", vn_province_year) %>%
    unlist %>% unique
  testthat::expect_equal(test3, "2008-2020")

  vn_8082 <- hist_map("Vietnam", vn_province, vn_history,
                       from = 1980, to = 1982)
  test4 <- vn_8082 %>%
    purrr::map("province") %>%
    purrr::discard(is.null) %>%
    purrr::map(data.frame) %>%
    purrr::map(dictionary::match_pattern, ".x..i..", vn_province_year) %>%
    unlist %>% unique
  testthat::expect_equal(test4, "1979-1990")

})
