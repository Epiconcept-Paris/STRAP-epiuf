library(epiuf)

test_that("Blanck correctly identified as NA", {
  expect_equal(validDate(""),NA)
})

# to be added after bug correction
# test_that("Standard Date correctly identified", {
#   expect_equal(validDate("2020-11-5"),NA)
# })

