library(epiuf)

test_that("externalFile to found data", {
  expect_output(externalFile("noneforsure"),"exists")
  expect_match(externalFile("excelfile.xlsx"),"excel")
})

