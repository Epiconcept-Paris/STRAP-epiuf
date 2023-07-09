testthat::test_that("getMax", {
  # Testing numeric
  expect_equal(getMax(1:25), 25)
  expect_equal(getMax(seq(123, 5, by = -2)), 123)
  # Testing float
  expect_equal(getMax(seq(123, 5, length.out = 200)), 123)
  #Testing with NA and NULL
  expect_equal(getMax(1:12, NA, NA, NULL, 33:24, NA), 33)
  # Testing logical
  expect_equal(getMax(TRUE, NA, TRUE, FALSE), 1)
  # Testing dates
  expect_equal(getMax(seq(as.Date("2023-01-01"), 
                          as.Date("2023-12-31"), 
                          by = "week")), as.Date("2023-12-31"))
  # Testing characters
  expect_equal(getMax(month.name), "September")
  expect_equal(getMax(LETTERS, NA, month.abb), "Z")
  # Testing full of NA
  expect_equal(getMax(rep(NA, 3896)), NA)
  expect_equal(getMax(rep(NaN, 4)), NA)
  expect_equal(getMax(rep(NULL, 557)), NA)
})


testthat::test_that("getMin", {
  # Testing numeric
  expect_equal(getMin(1:25), 1)
  expect_equal(getMin(seq(123, 5, by = -2)), 5)
  # Testing float
  expect_equal(getMin(seq(123, 5, length.out = 200)), 5)
  #Testing with NA and NULL
  expect_equal(getMin(1:12, NA, NA, NULL, 33:24, NA), 1)
  # Testing logical
  expect_equal(getMin(TRUE, NA, TRUE, FALSE), 0)
  # Testing dates
  expect_equal(getMin(seq(as.Date("2023-01-01"), 
                          as.Date("2023-12-31"), 
                          by = "week")), as.Date("2023-01-01"))
  # Testing characters
  expect_equal(getMin(month.name), "April")
  expect_equal(getMin(LETTERS, NA, month.abb), "A")
  # Testing full of NA
  expect_equal(getMin(rep(NA, 3896)), NA)
  expect_equal(getMin(rep(NaN, 4)), NA)
  expect_equal(getMin(rep(NULL, 557)), NA)
})


testthat::test_that("getMean", {
  # Testing numeric
  expect_equal(getMean(1:25), 13)
  expect_equal(getMean(seq(123, 5, by = -2)), 64)
  # Testing float
  expect_equal(getMean(seq(123, 5, length.out = 200)), 64)
  #Testing with NA and NULL
  expect_equal(getMean(c(1:12, NA, NA, NULL, 33:24, NA)), 16.5)
  # Testing logical
  expect_equal(getMean(c(TRUE, NA, TRUE, FALSE)), 2/3)
  # Testing dates
  expect_equal(getMean(seq(as.Date("2023-01-01"), 
                          as.Date("2023-12-31"), 
                          by = "week")), as.Date("2023-07-02"))
  # Testing characters
  expect_warning(getMean(month.name))
  expect_warning(getMean(c(LETTERS, NA, month.abb)))
  # Testing full of NA
  expect_equal(getMean(rep(NA, 3896)), NA)
  expect_equal(getMean(rep(NaN, 4)), NA)
  expect_equal(getMean(rep(NULL, 557)), NA)
})


testthat::test_that("getMedian", {
  # Testing numeric
  expect_equal(getMedian(1:25), 13)
  expect_equal(getMedian(seq(123, 5, by = -2)), 64)
  # Testing float
  expect_equal(getMedian(seq(123, 5, length.out = 200)), 64)
  #Testing with NA and NULL
  expect_equal(getMedian(c(1:12, NA, NA, NULL, 33:24, NA)), 11.5)
  # Testing logical
  expect_equal(getMedian(c(TRUE, NA, TRUE, FALSE)), TRUE)
  # Testing dates
  expect_equal(getMedian(seq(as.Date("2023-01-01"), 
                           as.Date("2023-12-31"), 
                           by = "week")), as.Date("2023-07-02"))
  # Testing characters
  expect_warning(getMedian(month.name))
  expect_warning(getMedian(c(LETTERS, NA, month.abb)))
  # Testing full of NA
  expect_equal(getMedian(rep(NA, 3896)), NA)
  expect_equal(getMedian(rep(NaN, 4)), NA)
  expect_equal(getMedian(rep(NULL, 557)), NA)
})
