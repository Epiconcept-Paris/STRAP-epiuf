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


#' Enhanced File Listing with Support for Wildcard Patterns
#'
#' This function is a wrapper around \code{list.files} from base R, providing additional options
#' such as automatic conversion from glob to regex patterns, this function supports direct use of standard 
#' directory patterns like "*" and "?" for matching files. Full names is enabled automatically when recursive search is activated.
#'
#' @param path Character. The path where to look for the files. Defaults to the current directory.
#' @param pattern Character. The pattern to match the files against. Supports both glob and regex patterns. Defaults to "*".
#' @param fullnames Logical. Whether to return full names including the path. Defaults to FALSE.
#' @param recursive Logical. Whether to list files recursively. Defaults to FALSE.
#' @param regex Logical. Whether the provided pattern is a regex pattern. Defaults to FALSE.
#'
#' @return Character vector. A vector of file names that match the criteria.
#' @export
#' @examples
#' \dontrun{
#' listFiles(path = "path/to/directory", pattern = "*.txt")
#' listFiles(path = "path/to/directory", pattern = "file[0-9]\\.txt", regex = TRUE)
#' }

listFiles  <- function(path =".", pattern="*",fullnames =FALSE, recursive=FALSE, regex=FALSE) 
{
    if(recursive) fullnames <- TRUE
    if( ! regex) pattern <- glob2rx(pattern)
    list.files(path,pattern ,ignore.case = TRUE, full.names = fullnames,
             recursive= recursive)
}


#' Extract File Extension From File Path or URL
#'
#' This function takes a file path or URL and returns the file extension.
#' Unlike functions like \code{tools::file_ext}, this function handles both paths and URLs.
#'
#' @param text Character. The full path or URL of the file.
#'
#' @return Character. The extension of the file. Returns an empty string if no extension is found.
#' @export
#' @examples
#' fileExt("path/to/file.txt")  
#' fileExt("https://example.com/file.csv") 
#' fileExt("no_extension_file") 
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

#' Extract File Base Name Without Extension From File Path or URL
#'
#' This function takes a file path or URL and returns the base name of the file without its extension.
#' Unlike \code{basename} from base R, which returns the file name with the extension, this function
#' directly provides just the base name, making it easier to manipulate or compare file names.
#'
#' @param text Character. The full path or URL of the file.
#'
#' @return Character. The base name of the file without the extension.
#' @export
#' @examples
#' fileName("path/to/file.txt")  # Returns "file" instead of "file.txt"
#' fileName("https://example.com/file.csv")  # Returns "file" instead of "file.csv"
#' 
fileName <- function(text) {
  name <- basename(text)
  x <- strsplit(name, "\\.")
  x[[1]][1]
}


#' Locate External File Across Different Paths
#'
#' This function takes an external file name and searches for its existence across different possible locations.
#' It first checks the current working directory, then the package's "extdata" directory, and finally a relative "inst/extdata" path.
#' Stop script with an error if the file cannot be found in any of the locations.
#'
#' @param extfile Character. The name of the external file to locate.
#'
#' @return Character. The path of the located external file.
#' @export
#' @examples
#' \dontrun{
#' externalFile("data.csv")  # Searches for "data.csv" in various locations
#' }
#' 
#' @seealso \code{\link{file.exists}}, \code{\link{system.file}}

externalFile <- function(extfile) {
  result <- extfile
  # Assume it is in working directory / fullname is given
  # if not , is it external data (while building package from vignette)
  if (! file.exists(result)) {
    result <- file.path(paste0(getwd(),"/inst/extdata"),extfile)
  }
  # if not , is it external data in installed package (if installed)
  if (!file.exists(result) ) {
    result <- system.file("extdata", extfile, package = "epiuf")
    result <- ifelse(result=="",extfile,result)
  }  
  # if nowhere we stop
  if(!file.exists(result)) {
    bye(paste0("File \"",result,"\" doesn't exists in package external data"))
  } else 
  # otherwise we return the path
  return(result)
} 

#' Replace Substring in a Given String
#'
#' This function replaces occurrences of a given substring (`pattern`) in a text (`intext`) with a new substring (`replacement`).
#' You can also specify if you want to replace only whole words and whether to ignore case.
#'
#' @param pattern The substring that you want to replace.
#' @param replacement The new substring that will replace `pattern`.
#' @param intext The input text where you want to replace the substring.
#' @param word Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#'
#' @return A string with the replaced substrings.
#' @export
#' @examples
#' replaceStr( "world", "everyone","Hello world")
#' replaceStr("WORLD", "everyone","Hello world",  ignore.case = TRUE)
#' replaceStr( "world", "everyone","Hello world", word = TRUE)
replaceStr <- function( pattern, replacement,intext, word=FALSE, ignore.case = TRUE ) {
  if(word) 
  {
    pattern = paste0("\\b",pattern,"\\b")
  }
  strresult = gsub(pattern, replacement, intext, ignore.case)
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
#' @param word Logical. If TRUE, only whole words will be replaced. Defaults to TRUE.
#' @param ignore.case Logical. If TRUE, the function performs a case-insensitive search. Defaults to FALSE.
#' @param listonly Logical. If TRUE, instead of modifying the files, a summary of the changes that would be made 
#' is displayed.. This is used to check changes before applying them. Defaults to FALSE.
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
           word = TRUE,
           ignore.case = FALSE,
           listonly=FALSE) {
    ext <- tolower(fileExt(filename))
    name <- fileName(filename)
    if (file.exists(filename)) {
      # file exists.. let's go
      if (ext %in% c("csv","r", "txt"))  {
          txtFindReplace(filename, pattern ,replacement,word , ignore.case, listonly)
      }
      if (ext %in% c("xlsx"))  {
        xlsxFindReplace(filename, pattern, replacement,word , ignore.case, listonly)
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
#' @param word Logical. If TRUE, only whole words will be replaced. Defaults to TRUE.
#' @param ignore.case Logical. If TRUE, the function performs a case-insensitive search. Defaults to FALSE.
#' @param listonly Logical. If TRUE, the function only lists the files that would be modified without actually
#'  modifying them. Defaults to FALSE.
#'
#' @return NULL. The function modifies the text file in-place and prints the exact number of changes made.
#' @export
#' @examples
#' \dontrun{
#' txtFindReplace("example.txt", "oldText", "newText")
#' txtFindReplace("example.txt", c("oldText1", "oldText2"), "newText")
#' }
#' @seealso 
#' \code{\link{fileFindReplace}}, \code{\link{xlsxFindReplace}}

txtFindReplace <- function(filename, pattern, replacement,word = TRUE, ignore.case=FALSE, listonly = FALSE) {
  FileContents <- readLines(filename,warn = FALSE)
  result <- 0
  for (i in 1:length(pattern)) {
    SearchedWord <- pattern[i]
    # we allow multiple searched text for on only replacement 
    ReplaceWord <- ifelse((length(replacement<= i)),replacement[i],replacement[1])
    # if whole word we past regex mark for word only
    if(word) SearchedWord <- paste0("\\b",SearchedWord,"\\b")
    # result <- grep(x = FileContents, pattern = SearchedWord,ignore.case)
    result <- result + charCount(pattern = SearchedWord,FileContents,ignore.case)
    FileContents <- gsub(x = FileContents, pattern = SearchedWord,replacement = ReplaceWord,ignore.case)
  }
  # modified lines are writen on file 
  if(!listonly) cat(FileContents, file = filename, sep = "\n")
  # and a message is displayed with number of changes 
  catret("File ",filename," ",result," Changes ")
  result
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
#' @param filename Character or character vector. The path of the directory, a single file, or a list of files.
#' @param pattern Character vector. A vector of patterns to search for in the files.
#' @param replacement Character vector. A vector of replacements for the patterns found.
#' @param word Logical. If TRUE, only whole words will be replaced. Defaults to FALSE.
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
#' \code{\link{fileFindReplace}}, \code{\link{xlsxFindReplace}},\code{\link{txtFindReplace} }

filesFindReplace <- function(filename,pattern,replacement,word = FALSE,ignore.case=FALSE,listonly=FALSE) {
      # We have only one then we can see what it is 
    if(length(filename)==1)
    { 
      # this file exists ?
      if(file.exists(filename)) {
         # Is it a directory ?
         if(file.info(filename)[["isdir"]] )
         {
             RScripts <- list.files(path = filename, pattern = "\\.R$",ignore.case = TRUE)
         }   
          # if not a directory, then a simple file 
         else  {
            # we can run FindReplace on that one ! 
           fileFindReplace(filename, pattern, replacement,word, ignore.case, listonly)
            # and we clear current RScripts
            RScripts <- NA
         }
      }   
      # may be a pattern ? 
      else {
        # we try to retrieve a list of files corresponding to filename as a pattern
        RScripts <- listFiles(path = ".", pattern = filename)
      }
    }   
  # We have more than one then each should be tested recursively 
  else RScripts <- filename
  
  # Now what do we have ?
  if (length(RScripts)>0 ) {
    for (RScript in RScripts) {
      if(! is.na(RScript)) 
         filesFindReplace(RScript, pattern, replacement, word, ignore.case,listonly)
    }
  }
  if(listonly){
    warning("While listonly is set to TRUE,changes are counted but not applied")
  }
}


#' Find and Replace Text in an Excel File
#'
#' This function reads an Excel file and replaces specified text (`oldname`) with a new text (`newname`) across all sheets.
#' You can specify whether to replace only whole words and whether to perform case-sensitive or case-insensitive replacements.
#'
#' @param xlsxName The name (including path) of the Excel (.xlsx) file to modify.
#' @param pattern The text to search for in the Excel file.
#' @param replacement The text that will replace `oldname`.
#' @param word Logical, if TRUE only whole words will be replaced.
#' @param ignore.case Logical, if TRUE the function ignores case.
#' @param listonly Logical. If TRUE, the function only lists the files that would be modified without actually
#'  modifying them. Defaults to FALSE.
#'
#' @return No return value; the function modifies the Excel file in-place and prints a message indicating the modified file.
#' @export
#' @importFrom openxlsx read.xlsx
#' @examples
#' \dontrun{
#' xlsxFindReplace("example.xlsx", "oldText", "newText")
#' }
xlsxFindReplace <-
  function(xlsxName,
           pattern,
           replacement,
           word = FALSE,
           ignore.case = TRUE ,
           listonly = FALSE)
  {
    wb <- loadWorkbook(xlsxName)
    # Number of sheets in the Excel file
    num_sheets <- length(names(wb))
    result <- 0
    for (i in 1:length(pattern)) {
      SearchedWord <- pattern[i]
      if (word)
        SearchedWord <- paste0("\\b", SearchedWord, "\\b")
      
      # Loop through each sheet
      for (i in 1:num_sheets) {
        # Read the sheet into a data frame
        df <- openxlsx::read.xlsx(xlsxName, sheet = i)
        # we count how many should be changed
        result <-
          result + charCount(SearchedWord, df, ignore.case)
        # Replace 'SearchedWord' with 'replacement' in all cells
        df[] <- lapply(df, function(col) {
          replaceStr( SearchedWord, replacement,col, ignore.case)
        })
        # Write the modified data frame back to the same sheet
        # write.xlsx(df, full_path, sheet = i, overwriteSheet = TRUE)
        openxlsx::writeData(wb, sheet = i, df)
      }
    }
    if (!listonly) {
      openxlsx::saveWorkbook(wb, xlsxName, overwrite = TRUE)
    }
    # and a message is displayed with number of changes
    catret("File ", xlsxName, " ", result, " Changes ")
    result
  }




# END of SCRIPT  --------------------------------------------------------

