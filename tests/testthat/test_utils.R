library(epiuf)

test_that("char count ok", {
  expect_equal(charCount(";","test;test;"),2)
})

test_that("no char count ok", {
  expect_equal(charCount(";","test test"),0)
})

test_that("File ext found", {
  expect_equal(fileExt("tira.dta"),"dta")
})

test_that("File name found", {
  expect_equal(fileName("c:/test/tira.dta"),"tira")
})

test_that("File ext found in path", {
  expect_equal(fileExt("users/test/data.tira.dta"),"dta")
})

test_that("File dta load ", {
  gastro5 <- readData("gastro5.dta")
  expect_equal(is.data.frame(gastro5),TRUE)
  expect_equal(exists("gastro5"),TRUE)
})

test_that("File csv load ", {
  tira <- readData("tira.csv")
  expect_equal(is.data.frame(tira),TRUE)
  expect_equal(is.factor(tira$sex),FALSE)
})

test_that("File csv load with factor ", {
  tira <- readData("tira.csv",factorise=TRUE)
  expect_equal(is.data.frame(tira),TRUE)
  expect_equal(is.factor(tira$sex),TRUE)
})

test_that("right of text correct", {
  expect_equal(right("dummy_test",4),"test")
})

