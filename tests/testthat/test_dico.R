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

test_that("Check Dico",{
  # create example dataset for testing
  df <- data.frame(checkbox_var = c("0|2", "1|2|3", "3|1|2", "0", "2", "3", "0|1", "2|3", NA_character_)
                   ,number_var = c(0,1,2,3,0,1,2,3,NA_real_)
                   ,character_var = c("0","1","nd","3","","1","2","3",NA_character_)
  )
  expect_output(checkDico(df, checkbox_var, c(0,1,2,3)),"matches dico")
  expect_output(checkDico(df, number_var, c(0,1,2,3)),"matches dico")
  expect_output(checkDico(df, character_var, c(0,1,2,3)),"matches dico")

  expect_output(checkDico(df, checkbox_var, c(0,1,2,3,4)),"matches dico")
  expect_output(checkDico(df, number_var, c(0,1,2,3,4)),"matches dico")
  expect_output(checkDico(df, character_var, c(0,1,2,3,4)),"matches dico")

  expect_output(checkDico(df, checkbox_var, c(0,1,2)),"contains")
  expect_output(checkDico(df, number_var, c(0,1,2)),"contains")
  expect_output(checkDico(df, character_var, c(0,1,2)),"contains")
  
    
})

