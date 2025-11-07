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


#' setPath
#' 
#' Set a named path to avoid absolute path in R scripts. Creates a short cut to folders and files. 
#'
#' @param pathname Label attributed to the path, specified in character string (e.g., \code{"DATA"}). 
#' @param path Absolute (or relative) path to the desired folder or file that you want linked with the \code{pathname}
#' (e.g., \code{"C:/R/Project/data"}). See function \code{'pathToFile()'}.
#' @param makedir If \code{"Force"} path will be created if it doesn't exist. <br>
#' If \code{"Never"}, path will not be created and a warning will pop if path is missing. <br>
#' If set to \code{"Ask"}, a prompt will ask for confirmation before creating the missing directory. 
#'
#' @return Defined path 
#' @export
#' @seealso [pathToFile()] and [getPath()]
#'
#' @examples
#' \dontrun{
#' # Set a name/keyword associated to a specific absolute path
#' setPath(pathname = "SOURCES", 
#'         path = "C:/dev/Resources", 
#'         makedir = "Never")
#' # Check path is properly set 
#' getPath("SOURCES")
#' 
#' # Set a name/keyword associated to a specific relative path
#' setPath(pathname = "DATA", 
#'         path = "./data", # Note './' for relative paths
#'         makedir = "Never")
#' # Checking the path is properly set
#' getPath("DATA")
#' 
#' # Importing file (with base R functions)
#' df <- readRDS(file.path(getPath("DATA"), "df.rds")) 
#' ## or with epiuf functions
#' df <- readData(pathToFile("DATA", "df.rds"))
#' }
#' 
#' 
setPath <-  function(pathname, 
                     path, 
                     makedir = c("Ask", "Force", "Never")) {
  
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
#' This function retrieves a named Path previously defined in \code{setPath()}. 
#'
#' @param pathname Label attributed to the path, specified in character string (e.g., \code{"DATA"}). 
#'
#' @return The path saved under pathname label.
#' @export
#' @seealso [setPath()] and [pathToFile()]
#' 
#' @examples
#' \dontrun{
#' # First set the path directory and give it a label 
#' setPath(pathname = "DATA", 
#'         path = "./data", 
#'         makedir = "Never")
#' 
#' # Get the file path using the function 
#' getPath("DATA")
#' 
#' # Importing file using getPath
#' df <- readData(pathToFile("DATA", "df.rds"))
#' }
#' 
getPath <-  function(pathname) {
  pathname <- paste0("PATH_", pathname)
  r <- getEpiOption(pathname)
  if(is.null(r)){ 
     cat("Path not defined with setPath : ", pathname, "\n")
  }
  return(r)
}


#' sourceFile
#'
#' 
#' Source a file from a previously set path and run it. 
#' It uses the label of the path (\code{pathname}) and the name of the file 
#' (\code{filename}) of which you want to run. 
#' This function is a wrapper for 'source()' base R function.
#' 
#' 
#' @param pathname  Label attributed to the path where to find the file to source, 
#' specified in character string (e.g., \code{"SCRIPTS"}).  
#' @param filename  The name of the file to source in character string
#' (if it contains a path, it will be added to the specified path (\code{pathname}).
#'
#' @return No return value
#' @export
#' @seealso [setPath()]
#' @examples
#' \dontrun{
#' # Set an example path named 'SCRIPTS' 
#' setPath(pathname = "SCRIPTS", 
#'        path = "./scripts", 
#'        makedir = "Never")
#' 
#' # Source an R script from the specified path 
#' sourceFile("SCRIPTS", "import_data.R")
#' }
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
#' Constructs an absolute path to a file using a previously set saved path name 
#' (see \code{setPath()}) and the file name.
#' 
#'
#' @param pathname Label attributed to the path, specified in character string (e.g., \code{"DATA"}).
#' @param filename Name of the file to retrieve, may contain sub directory 
#'
#' @return The full path to the file 
#' @seealso [setPath()]
#' @export
#' @examples
#' \dontrun{
#' # Set the path directory and give it a label 
#' setPath(pathname = "DATA", 
#'         path = "./data")
#' 
#' # Construct the full file path
#' pathToFile("DATA", "df.rds")
#' 
#' # Importing file using getPath
#' df <- readData(pathToFile("DATA", "df.rds"))
#' 
#' # Set another example path 
#' setPath(pathname = "SCRIPTS", 
#'         path = "./scripts")
#' 
#' # Construct the full path to a given file 
#' pathToFile("SCRIPTS", "import_data.R")
#' }
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
