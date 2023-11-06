library(epiuf)

test_that("externalFile to found data", {
  expect_error(externalFile("noneforsure"),regexp="exists")
  expect_match(externalFile("excelfile.xlsx"),"excel")

})


