library(epiuf)

test_that("fileFindReplace", {
  file1 <- "temp_text1.R"
  lines <- c(
    "# test script for findAndReplace",
    "# word to be replaced : oldword",
    "#  old_word",
    "#  oldword --  oldword)")
  
  writeLines(lines, file1)
  expect_output(suppressWarnings(filesFindReplace(file1,pattern="oldword",replacement = "newword",listonly=TRUE)),"3")
  expect_warning(filesFindReplace(file1,pattern="oldword",replacement = "newword",listonly=TRUE))
  expect_output(filesFindReplace(file1,pattern="oldword",replacement = "newword"),"3")
    if(file.exists(file1)) r <- file.remove(file1) 
})
