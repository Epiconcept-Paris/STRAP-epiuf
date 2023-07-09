testthat::test_that("calYear", {
  expect_equal(calYear(as.Date("2019-12-31")), 2019)
})

testthat::test_that("abrvMonth", {
  expect_equal(abrvMonth(as.Date("2019-12-31")), 
               format(as.Date("2019-12-31"), format="%b"))
})

testthat::test_that("Month", {
  expect_equal(Month(as.Date("2019-12-31")), 12)
  expect_equal(Month(as.Date("2020-06-30")), 6)
})

testthat::test_that("iso dates functions", {
  # isoYear
  expect_equal(isoYear(as.Date("2019-12-31")), 2020)
  expect_equal(isoYear(as.Date("2020-12-31")), 2020)
  
  # isoWeek
  expect_equal(isoWeek(as.Date("2019-12-31")), 1)
  expect_equal(isoWeek(as.Date("2020-12-31")), 53)
  
  # isoYearWeek
  expect_equal(isoYearWeek(as.Date("2019-12-31")), "2020w01")
  expect_equal(isoYearWeek(as.Date("2020-12-31")), "2020w53")
  expect_equal(isoYearWeek(as.Date("2015-12-31"), 
                           weekformat = "-W"), "2015-W53")
})

testthat::test_that("countIsoWeeks", {
  expect_equal(as.numeric(countIsoWeeks(date = as.Date("2019-01-31"),
                                        origin = as.Date("2019-01-01"))), 4)
  expect_equal(as.numeric(countIsoWeeks(date = as.Date("2023-02-25"),
                                        origin = "2023-01-01")), 8)
})

testthat::test_that("lastDateMonth", {
  expect_equal(lastDateMonth("dec2020"), as.Date("2020-12-31"))
  expect_equal(lastDateMonth("feb2023"), as.Date("2023-02-28"))
})

