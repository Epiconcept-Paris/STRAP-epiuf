#
# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------



fileExt <- function(text) {
  x <- strsplit(text, "\\.")
  i <- length(x[[1]])
  ext <- ""
  if (i > 1) {
    ext <- x[[1]][i]
  }
  ext
}

fileName <- function(text) {
  name <- basename(text)
  x <- strsplit(name, "\\.")
  x[[1]][1]
}




#' setPath
#' 
#' Set a named path to avoid absolute path in R scripts
#'
#' @param pathname pathname Label 
#' @param path absolute path to associate with the pathname
#'
#' @return Previous defined path 
#' @export
#'
#' @examples
#' 
#' setPath("sources","c:/dev/Rsources")
#' 
setPath <-  function(pathname,path) {
  s_op <- deparse(substitute(pathname))
  # if pathname is a variable wich contain char, we use content of pathname
  ok <- FALSE
  tryCatch(
    if (is.character(pathname)) {
      s_op <- pathname
      ok <- TRUE
    }
    , error = function(c) { }
  )
  
  invisible(setEpiOption(paste0("PATH_",s_op),path))
}


#' getPath
#' 
#' Retrieve a named Path 
#'
#' @param pathname Path label 
#'
#' @return The path saved under pathname label
#' @export
#'
#' @examples
#' getPath("sources")
#' 
#' 
getPath <-  function(pathname) {
  pathname <- paste0("PATH_",pathname)
  invisible(getEpiOption(pathname))
}


#' sourceFile
#'
#' sourcefile function is a wrapper for source which will later allow some test and feebacks
#' sourceFile source a file from the "scripts" path as set by setPath("scripts","absolute path")
#' 
#' @param pathname  pathname as label  
#' @param filename  name of the file to source (if it contain a path, this one will be added to the scripts path)
#'
#' @return nothing
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".R")
#' cat("# comment", file = file, sep = "\n")
#' setPath("scripts","")
#' sourceFile("scripts",file)
#' unlink(file) # tidy up
#' 
sourceFile <- function(pathname, filename )  {
  s_op <- deparse(substitute(pathname))
  # if op is a variable wich contain char, we use content of op
  ok <- FALSE
  tryCatch(
    if (is.character(pathname)) {
      s_op <- pathname
      ok <- TRUE
    }
    , error = function(c) { }
  )
  
  fullname <- pathToFile(s_op,filename)
  if ( is.null(fullname) ) {
    stop(paste0("Path :",s_op," is not defined in 'SetPath()', verify the config file"))
  } 
  else { 
    if ( file.exists(fullname) ) {
      source(fullname) 
    }
    else {
      stop(paste0("File :",fullname," doesn't exist"))
    }
  }
}

#' pathToFile
#' 
#' construct an absolute path to a file using file name and saved/labeled path
#'
#' @param pathname Label for saved path
#' @param filename Name of the file to retreive, may contain subdir 
#'
#' @return the full name of the file 
#' @export
#'
#' @examples
#' pathToFile("data","cleanedrecords.csv")
#' 
#' 
pathToFile <- function(pathname, filename) {
  r <-  getPath(pathname) 
  if (is.null(r)) {
    cat("Path not defined : ",pathname,"\n")
  } 
  else {
    r <- file.path(r,filename)  
  }
  return(r)
}



# END of SCRIPT  --------------------------------------------------------

