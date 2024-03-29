library(epiuf)

test_that("applyNA", {
  # create example dataset for testing
  df <- data.frame(CharacterVar = c("unknown", "do not know", "other", "dnk", "na", "nd", "nsp", "ne zna", 
                                    "testdnk", "dnktest", "test", "not test", "3", "4"), 
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
  
  # check for ignore.case
  df <- data.frame(CharacterVar = c("unknown", "UNKNOWN")  )
  expect_equal(sum(is.na(applyNA(df, CharacterVar))),2)
  expect_output(sum(is.na(applyNA(df, CharacterVar))),": 2")
  
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

test_that("Collapse Var",{
  # create example dataset for testing
  df <- data.frame(checkbox_bar = c("0|2", "1|2|3", "3|1|2", "0", "2", "3", "0|1", "2|3", "")
                   ,checkbox_comma = c("0,2", "1,2,3", "3,1,2", "0", "2", "3", "0,1", "2,3", "")
                   ,checkbox_slash = c("0/2", "1/2/3", "3/1/2", "0", "2", "3", "0/1", "2/3", "")
  )
  expect_equal(collapseVar(df, checkbox_bar, c(0,1,2,3)),c(0,1,1,0,2,3,0,2,NA))
  expect_equal(collapseVar(df, checkbox_comma, c(0,1,2,3)),c(0,1,1,0,2,3,0,2,NA))
  expect_equal(collapseVar(df, checkbox_slash, c(0,1,2,3)),c(0,1,1,0,2,3,0,2,NA))
  expect_error(collapseVar(df, checkbox_oups, c(0,1,2,3)),label="wrong column name")
 
  expect_equal(collapseVar(df, checkbox_bar, c(0,1,2,3,4)),c(0,1,1,0,2,3,0,2,NA))

  expect_output(collapseVar(df, checkbox_bar, c(0,1,2)),"missing")
  
  
})  

test_that("Expand Var",{
  # create example dataset for testing
  
data <- data.frame(Id = 1:4 , 
                   Vaccs = c("pfizer,moderna"," ", "pfizer", "moderna"))
brand <- list("pfizer"="pfizer",
              "moderna"="moderna"
              )
data2 <-  expandVar(data,Vaccs,brand)

expect_match(names(data2)[4],"Vaccs_moderna")
expect_equal(data2[1,4],1)
expect_equal(data2[3,4],0)
expect_equal(data2[4,4],1)

file1 <- externalFile("genericdictionary.xlsx")
openDictionary(file1)
data2 <- expandVarAll(data)

expect_match(names(data2)[4],"Vaccs_moderna")
expect_equal(data2[1,4],1)
expect_equal(data2[3,4],0)
expect_equal(data2[4,4],1)


}) 

test_that("Base dico function",{
  file1 <- externalFile("genericdictionary.xlsx")
  openDictionary(file1)
  val <- getAnyDictionaryValue("gen4",searchcolumn = "generic_name",value="description")
  expect_equal(val,"Binary variable")
  val <- getAnyDictionaryValue("gen12",searchcolumn = "generic_name",value="description")
  expect_equal(val,NA,label="Missing variable should give NA")
  expect_warning(val <- getAnyDictionaryValue("gen4",searchcolumn = "badname",value="description"))
  expect_equal(val,NA,label="Wrong parameters should give NA")
  
}) 
