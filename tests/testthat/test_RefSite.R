testthat::test_that("getRefSiteID", {
  expect_equal(getRefSiteID(Label = "Austria"), "AT")
  expect_equal(getRefSiteID(LocationName = "Bosnia and Herzegovina"), "BA")
  expect_equal(getRefSiteID(Label = "The Czech Republic", LocationName = "Czechia"), "CZ")
})

testthat::test_that("getRefSiteLabel", {
  expect_equal(getRefSiteLabel(ID = "SI"), "Slovenia")
})