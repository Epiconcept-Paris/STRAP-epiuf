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

#' Extract File Base Name Without Extension
#'
#' This function takes a file path or URL and returns the base name of the file without the extension.
#'
#' @param text Character. The full path or URL of the file.
#'
#' @return Character. The base name of the file without the extension.
#' @export
#' @examples
#' \dontrun{
#' fileName("path/to/file.txt")
#' fileName("https://example.com/file.csv")
#' }
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
#' The function can work with multiple patterns and replacements, and can also perform 
#' case-sensitive or case-insensitive searches.
#'
#' @param filename Character. The name of the file to modify.
#' @param pattern Character vector. A vector of patterns to search for in the file.
#' @param replacement Character vector. A vector of replacements for the patterns found.
#' @param wholeword Logical. If TRUE, only whole words will be replaced. Defaults to TRUE.
#' @param ignore.case Logical. If TRUE, the function performs a case-insensitive search. Defaults to FALSE.
#'
#' @return NULL. The function modifies the file in-place.
#' @export
#' @examples
#' \dontrun{
#' fileFindReplace("example.txt", "oldText", "newText")
#' fileFindReplace("example.txt", c("oldText1", "oldText2"), "newText")
#' }
#' @seealso 
#' \code{\link{txtFindReplace}}, \code{\link{xlsxFindReplace}}
fileFindReplace <-
  function(filename,
           pattern,
           replacement,
           wholeword = TRUE,
           ignore.case = FALSE) {
    ext <- tolower(fileExt(filename))
    name <- fileName(filename)
    if (file.exists(filename)) {
      # file exists.. let's go
      if (ext %in% c("csv","r", "txt"))  {
          txtFindReplace(filename, pattern ,replacement,wholeword , ignore.case)
      }
      if (ext %in% c("xlsx"))  {
        xlsxFindReplace(filename, pattern, replacement,wholeword , ignore.case)
      }
    }
    
  }


#' Find and Replace Text in a Text File
#'
#' This function reads a text file and replaces specified patterns with given replacements.
#' It allows for multiple patterns and replacements, and can perform case-sensitive or 
#' case-insensitive searches. It can also match whole words.
#'
#' @param filename Character. The name of the file to modify.
#' @param pattern Character vector. A vector of patterns to search for in the file.
#' @param replacement Character vector. A vector of replacements for the patterns found.
#' @param wholeword Logical. If TRUE, only whole words will be replaced. Defaults to TRUE.
#' @param ignore.case Logical. If TRUE, the function performs a case-insensitive search. Defaults to FALSE.
#'
#' @return NULL. The function modifies the text file in-place and prints the number of changes made.
#' @export
#' @examples
#' \dontrun{
#' txtFindReplace("example.txt", "oldText", "newText")
#' txtFindReplace("example.txt", c("oldText1", "oldText2"), "newText")
#' }
#' @seealso 
#' \code{\link{fileFindReplace}}, \code{\link{xlsxFindReplace}}

txtFindReplace <- function(filename, pattern, replacement,wholeword = TRUE, ignore.case=FALSE) {
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

#' Find and Replace Text in Files in a Directory or File List
#'
#' This function searches for specified patterns in files within a directory or a given list of files
#' and replaces them with given text. While optimized for R scripts, the function call fileFindReplace function 
#' that can work on any text-based files or xlsx Files.
#' 
#' The function supports case-sensitive or case-insensitive searches and can either perform the replacements
#' or just list the files that would be modified.
#'
#' @param directory Character or character vector. The path of the directory, a single file, or a list of files.
#' @param oldtext Character vector. A vector of patterns to search for in the files.
#' @param newtext Character vector. A vector of replacements for the patterns found.
#' @param wholeword Logical. If TRUE, only whole words will be replaced. Defaults to FALSE.
#' @param ignore.case Logical. If TRUE, the function performs a case-insensitive search. Defaults to FALSE.
#' @param listonly Logical. If TRUE, the function only lists the files that would be modified without actually
#'  modifying them. Defaults to FALSE.
#'
#' @return NULL. The function modifies the files in-place, or lists them if \code{listonly = TRUE}.
#' @export
#' @examples
#' \dontrun{
#' filesFindReplace("path/to/directory", "oldText", "newText")
#' filesFindReplace("path/to/file.txt", "oldText", "newText")
#' filesFindReplace(c("file1.txt", "file2.csv"), "oldText", "newText")
#' }
#' @seealso 
#' \code{\link{fileFindReplace}}, \code{\link{xlsxFindReplace},\code{\link{txtFindReplace} }

filesFindReplace <- function(directory,oldtext,newtext,wholeword = FALSE,ignore.case=FALSE,listonly=FALSE) {
    # We have only one then we can see what it is 
    if(length(directory)==1)
    { 
      # this file exists ?
      if(file.exists(directory)) {
         # Is it a directory ?
         if(file.info(directory)[["isdir"]] )
         {
             RScripts <- list.files(path = directory, pattern = "\\.R$",ignore.case = TRUE)
         }   
          # if not a directory, may be a simple file ?
         else  {
            # we can run FindReplace on that one ! 
            if(!listonly) {
               fileFindReplace(directory, oldtext, newtext,wholeword, ignore.case)
            }
            print(paste("Modified file: ", directory))
            # and we clear current RScripts
            RScripts <- NA
         }
      }   
      # may be a pattern ? 
      else {
        # we try to retrieve a list of files (with R by default)
        RScripts <- list.files(path = directory, pattern = "\\.R$",ignore.case = TRUE)
      }
    }   
  # We have more than one then each should be tested recursively 
  else RScripts <- directory
  
  # Now what do we have ?
  if (length(RScripts)>0 ) {
    for (RScript in RScripts) {
      if(! is.na(RScript)) 
         filesFindReplace(RScript, oldtext, newtext, wholeword, ignore.case)
    }
  }
  
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
}




# END of SCRIPT  --------------------------------------------------------

