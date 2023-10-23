#
# Project Name : STRAP
# Script Name  : utilsFile.R
# GitHub repo  : https://github.com/Epiconcept-Paris/STRAP-epiuf
# Summary      : File related Utility functions
# Date created : 
# Author       : G.Desve
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
#   Somme function to facilitate file manipulation
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' listFiles
#'
#' @param path  Path to search for files  "." mean current directory
#' @param pattern Pattern of files to be listed 
#' @param fullnames Should the function return full name with path or only file name
#' @param recursive If TRUE sub directories are explored (fullnames set to TRUE)
#' @param regex If FALSE, the default, the pattern is considered as global pattern 
#'              with only standard * and ? jokers 
#'
#' @return  Character list of found files 
#' @export
#'
#' @examples
#' listFiles(path = ".", pattern = "*.doc")
#' 
listFiles  <- function(path =".", pattern="*",fullnames =FALSE, recursive=FALSE, regex=FALSE) 
{
    if(recursive) fullnames <- TRUE
    if( ! regex) pattern <- glob2rx(pattern)
    list.files(path,pattern ,ignore.case = TRUE, full.names = fullnames,
             recursive= recursive)
}


#' fileExt
#'      Retrieve the file extension  
#'
#' @param text A filename 
#'
#' @return The file extension as character
#' @export
#'
#' @examples
#' 
#' myext <- fileExt("mydoc.pdf")
#' 
fileExt <- function(text) {
  x <- strsplit(text, "\\.")
  i <- length(x[[1]])
  ext <- ""
  if (i > 1) {
    ext <- x[[1]][i]
  }
  ext
}

# the file name whithout extension
#' fileName
#'
#' @param text A file name
#'
#' @return the filename without path or extension
#' @export
#'
#' @examples
#' fileName("briths.rds")
fileName <- function(text) {
  name <- basename(text)
  x <- strsplit(name, "\\.")
  x[[1]][1]
}


#' externalFile
#' 
#' This function is used mainly for vignettes. The file is searched 
#' - locally
#' - into extdata from package 
#' - into inst/extdata locally 
#'
#' @param extfile Filename
#'
#' @return The full path to access the founded file
#' @export
#'
#' @examples
#' externalFile("testbrand.csv")
externalFile <- function(extfile) {
  result <- extfile
  if (!file.exists(result) ) {
    result <- system.file("extdata", extfile, package = "epiuf")
    result <- ifelse(result=="",extfile,result)
  }  
  if (! file.exists(result)) {
    result <- file.path("../inst/extdata",extfile)
  }
  
  stopifnot(file.exists(result))
  return(result)
} 

#' Replace Substring in a Given String
#'
#' This function replaces occurrences of a given substring (`old`) in a text (`intext`) with a new substring (`new`).
#' You can also specify if you want to replace only whole words and whether to ignore case.
#'
#' @param intext The input text where you want to replace the substring.
#' @param old The substring that you want to replace.
#' @param new The new substring that will replace `old`.
#' @param word Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return A string with the replaced substrings.
#' @export
#' @examples
#' replaceStr("Hello world", "world", "everyone")
#' replaceStr("Hello world", "WORLD", "everyone", ignore.case = TRUE)
#' replaceStr("Hello world", "world", "everyone", word = TRUE)
replaceStr <- function(intext, old, new, word=FALSE, ignore.case = TRUE ) {
  if(word) 
  {
    old = paste0("\\b",old,"\\b")
  }
  strresult = gsub(old, new, intext, ignore.case)
  return(strresult)
}

#' Find and Replace Text in a File
#'
#' This function reads a file and replaces specified patterns with given replacements.
#' The function can work with multiple patterns and replacements, and can also perform case-sensitive or case-insensitive searches.
#'
#' @param filename The name of the file to modify.
#' @param pattern A vector of patterns to search for in the file.
#' @param replacement A vector of replacements for the patterns found.
#' @param wholeword Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return No return value; the function modifies the file in-place.
#' @export
#' @examples
#' \dontrun{
#' fileFindReplace("example.txt", "oldText", "newText")
#' fileFindReplace("example.txt", c("oldText1", "oldText2"), "newText")
#' }
fileFindReplace <- function(filename, pattern, replacement,wholeword = TRUE, ignore.case=FALSE) {
  FileContents <- readLines(filename,warn = FALSE)
  for (i in 1:length(pattern)) {
    SearchedWord <- pattern[i]
    # we allow multiple searched text for on only replacement 
    ReplaceWord <- ifelse((length(replacement<= i)),replacement[i],replacement[1])
    # if whole word we past regex mark for word only
    WW <- ifelse(wholeword,"\\b","") 
    SearchedWord <- paste0(WW,SearchedWord,WW)
    result <- grep(x = FileContents, pattern = SearchedWord,ignore.case)
    FileContents <- gsub(x = FileContents, pattern = SearchedWord,replacement = ReplaceWord,ignore.case)
  }
  cat(FileContents, file = filename, sep = "\n")
  cat("File ",filename," ",length(result)," Changes")
}


#' Batch Find and Replace in R Scripts within a Directory
#'
#' This function finds and replaces specified text in all R scripts (.R or .r) within a given directory.
#' 
#' @param directory The directory where R scripts are located.
#' @param oldtext The text to search for in R scripts.
#' @param newtext The text that will replace `oldtext`.
#' @param wholeword Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return No return value; the function modifies the files in-place.
#' @export
#' @examples
#' \dontrun{
#' globalFileReplace("./myRScripts/", "oldFunc", "newFunc")
#' }

globalFileReplace <- function(directory,oldtext,newtext,wholeword,ignore.case=FALSE) {
  RScripts <- list.files(path = directory, pattern = "\\.R$",ignore.case = TRUE)
  for (RScript in RScripts ) {
    fileFindReplace(RScript, oldtext, newtext,wholeword = TRUE, ignore.case)
  }
}

#' Batch Find and Replace in Excel Files within a Directory
#'
#' This function performs a find and replace operation for specified text across all Excel (.xlsx) files in a given directory.
#' You can specify whether to replace only whole words and whether to perform case-sensitive or case-insensitive replacements.
#'
#' @param dir_path The directory path where Excel (.xlsx) files are located.
#' @param oldname The text to search for in the Excel files.
#' @param newname The text that will replace `oldname`.
#' @param word Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return No return value; the function prints a message indicating that all files have been modified.
#' @export
#' @examples
#' \dontrun{
#' replaceInExcelAll("./excelFiles/", "oldText", "newText")
#' }
replaceInExcelAll <- function(dir_path, oldname, newname, word=FALSE, ignore.case = TRUE) {
  
  # List all Excel files in the directory
  excelFiles <- list.files(path = dir_path, pattern = "\\.xlsx$")
  
  # Loop through each Excel file
  for (file in excelFiles) {
    # Full path to the file
    fullPath <- paste0(dir_path, "/", file)
    xlsxFindReplace(fullPath,oldname,newname,word,ignore.case)
  }
  print("All files have been modified.")
}

#' Find and Replace Text in an Excel File
#'
#' This function reads an Excel file and replaces specified text (`oldname`) with a new text (`newname`) across all sheets.
#' You can specify whether to replace only whole words and whether to perform case-sensitive or case-insensitive replacements.
#'
#' @param xlsxName The name (including path) of the Excel (.xlsx) file to modify.
#' @param oldname The text to search for in the Excel file.
#' @param newname The text that will replace `oldname`.
#' @param word Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return No return value; the function modifies the Excel file in-place and prints a message indicating the modified file.
#' @export
#' @importFrom openxlsx read.xlsx
#' @examples
#' \dontrun{
#' xlsxFindReplace("example.xlsx", "oldText", "newText")
#' }
xlsxFindReplace <- function(xlsxName, oldname, newname, word=FALSE, ignore.case = TRUE )
{
  wb <- loadWorkbook(xlsxName)
  # Number of sheets in the Excel file
  num_sheets <- length(names(wb))
  
  # Loop through each sheet
  for (i in 1:num_sheets) {
    # Read the sheet into a data frame
    df <- openxlsx::read.xlsx(xlsxName, sheet = i)
    
    # Replace 'oldname' with 'newname' in all cells
    df[] <- lapply(df, function(col) {
      replaceStr(col, old = oldname, new = newname , word, ignore.case)
    })
    
    # Write the modified data frame back to the same sheet
    # write.xlsx(df, full_path, sheet = i, overwriteSheet = TRUE)
    writeData(wb, sheet = i, df)
  }
  saveWorkbook(wb, xlsxName, overwrite = TRUE)
  print(paste("Modified file:", xlsxName))
}




# END of SCRIPT  --------------------------------------------------------

