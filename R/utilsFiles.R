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



# END of SCRIPT  --------------------------------------------------------

