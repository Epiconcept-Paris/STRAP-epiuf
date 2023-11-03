library(epiuf)

test_that("applyNA", {
  # create example dataset for testing
  df <- data.frame(CharacterVar = c("unknown", "do not know", "other", "dnk", "na", "nd", "nsp", "ne zna", "testdnk", "dnktest", "test", "not test", "3", "4"), 
                   NumberVar = c(1, 2, 3, 4, 1, 2, 3, 4,1, 2, 3, 4, 1, 2)
  )

  expect_equal(sum(is.na(applyNA(df, CharacterVar))),7)
  expect_equal(sum(is.na(applyNA(df, CharacterVar, "^test$"))),1)
  expect_equal(sum(is.na(applyNA(df, CharacterVar, "^test$", join=TRUE))),8) 
  expect_equal(sum(is.na(applyNA(df, CharacterVar, "test", join=TRUE))),8) 
  expect_equal(sum(is.na(applyNA(df, CharacterVar, c("^test$","^other*")))),2) 
  expect_equal(sum(is.na(applyNA(df, CharacterVar, "test,other"))),2) 
  expect_equal(sum(is.na(applyNA(df, NumberVar, 3))),3)
  expect_equal(sum(is.na(applyNA(df, NumberVar, c(3,1)))),7)   
  expect_equal(sum(is.na(applyNA(df, NumberVar, "3,1"))),7) 
})
