testthat::test_that("getRefSiteID", {
  expect_error(getRefSiteID())
  expect_equal(getRefSiteID(Label = "Austria"), "AT")
  expect_equal(getRefSiteID(LocationName = "Bosnia and Herzegovina"), "BA")
  expect_equal(getRefSiteID(Label = "The Czech Republic", LocationName = "Czechia"), "CZ")
  expect_warning(getRefSiteID(Label = "Czechia"), "Sorry but no rows in RefSite match your search.")
})

testthat::test_that("getRefSiteLabel", {
  expect_error(getRefSiteLabel())
  expect_equal(getRefSiteLabel(ID = "SI"), "Slovenia")
  expect_warning(getRefSiteLabel(ID = "es"), "Sorry but no rows in RefSite match your search.")
})

testthat::test_that("getRefSiteLocName", {
  expect_error(getRefSiteLocName())
  expect_equal(getRefSiteLocName(ID = "TR"), "Turkey")
  expect_warning(getRefSiteLocName(ID = "ukk"), "Sorry but no rows in RefSite match your search.")
})