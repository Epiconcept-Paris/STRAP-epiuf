library(epiuf)

test_that("fileFindReplace", {
  # prep small file for tests
  file1 <- "temp_text1.R"
  lines <- c(
    "# test script for findAndReplace",
    "# word to be replaced : oldword",
    "#  old_word",
    "#  oldword --  oldword)")
  writeLines(lines, file1)

    # test for warning if listonly = TRUE
  expect_warning(filesFindReplace(file1,pattern="oldword",replacement = "newword",listonly=TRUE))
  # test output when listonly=TRUE
  expect_output(suppressWarnings(filesFindReplace(file1,pattern="oldword",replacement = "newword",listonly=TRUE)),"3")
  # 3 replacements should be done
  expect_output(filesFindReplace(file1,pattern="oldword",replacement = "newword"),"3")
  # nothing to do after first FindAndReplace
  expect_output(filesFindReplace(file1,pattern="oldword",replacement = "newword"),"0")

  #we restore file for next tests
  writeLines(lines, file1)
  expect_output(filesFindReplace(file1,pattern=c("oldword","script"),replacement = c("newword","text")),"4")
  lines2 <- readLines(file1)
  expect_equal(charCount("newword",lines2),3)
  expect_equal(charCount("text",lines2),1)
  
  # cleanup
  if(file.exists(file1)) r <- file.remove(file1) 
})
