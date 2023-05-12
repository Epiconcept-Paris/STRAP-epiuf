library(epiuf)

test_that("Blanck correctly identified as NA", {
 expect_equal(validDate(""),NA)
})


test_that("Standard Date correctly identified Year first", {
  expect_equal(validDate(datevar = "1960 01 15"),as.Date("1960-01-15"))
})

test_that("Standard Date correctly identified Year Last", {
  expect_equal(validDate(datevar = "15.01.1960"),as.Date("1960-01-15"))
})

test_that("Standard Date correctly identified Year 2 digit", {
  expect_equal(validDate(datevar = "15/01/60"),as.Date("1960-01-15"))
})

