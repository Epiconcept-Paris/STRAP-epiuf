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

#' listFiles
#'
#' @param path  Path to search for files  "." mean current directory
#' @param pattern Pattern of files to be listed 
#'
#' @return  Character list of found files 
#' @export
#'
#' @examples
#' listFiles(".", "*.doc")
#' 
listFiles  <- function(path =".", pattern="*") {
  list.files(path,pattern = glob2rx(pattern),ignore.case = TRUE )
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




#' setPath
#' 
#' Set a named path to avoid absolute path in R scripts
#'
#' @param pathname pathname Label 
#' @param path absolute path to associate with the pathname
#' @param makedir if "Force" path will be created if it doesn't exist, if "Never" path will not be created and 
#'                a warning will pop if path is missing. If set to "Ask" a prompt will ask for confirmation before 
#'                creating the missing directory 
#'
#' @return Previous defined path 
#' @export
#'
#' @examples
#' setPath("SOURCES","c:/dev/Rsources", makedir = "Never")
#' getPath("SOURCES")
#' 
#' # Setting a name/keyword associated to a specific path
#' setPath(pathname = "DATA", 
#'         path = "./data", # Note './' for relative paths
#'         makedir = "Never")
#' # Checking the path is properly set
#' getPath("DATA")
#' 
setPath <-  function(pathname, path, makedir = c("Ask","Force","Never")) {
  s_op <- deparse(substitute(pathname))
  # if pathname is a variable which contain char, we use content of pathname
  ok <- FALSE
  tryCatch(
    if (is.character(pathname)) {
      s_op <- pathname
      ok <- TRUE
    }
    , error = function(c) { }
  )
  if (missing(path)) stop("setPath: path argument is missing with no default for setPath")
  if (missing(makedir)) {
    makedir <- "ask"
  }
  # Lower cap makedir so that it is not case sensitive
  makedir <- tolower(makedir)
  if (!(makedir %in% c("ask","force","never"))) {
    warning("setPath: ", makedir, " is not a valid option for 'makedir' argument. Please check help.")
    makedir <- "ask"
  }
  if ( ! (path == "" | dir.exists(path)) ) {
    if (! makedir == "never") {
      result <- FALSE
      if (makedir == "ask") {
        cat(paste(path, "doens't exist."))
        result <- epiuf::yesno("Do you want to create it ?")
      }
      if (makedir == "force") result = TRUE
      if (!is.na(result) & result == TRUE) {
        dir.create(path, recursive = TRUE)
      }
    } else warning("setPath: ", path, " doesn't exist as directory")
  } 

  invisible(epiuf::setEpiOption(paste0("PATH_", s_op), path))
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
  r <- getEpiOption(pathname)
  if(is.null(r)){ 
     cat("Path not defined with setPath : ",pathname,"\n")
  }
  return(r)
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
  if (! is.null(r))   { 
    if ( ! r =="" ) {
      r <- file.path(r,filename) 
    } else r <- filename
  }
  return(r)
}


# END of SCRIPT  --------------------------------------------------------
