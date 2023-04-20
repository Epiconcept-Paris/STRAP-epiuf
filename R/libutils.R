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

#' loadLib
#'
#' library loading function which ask for confirmation before installing library
#'
#' @seealso For more details see the help vignette: \cr
#' \code{vignette("epiuf_package", package = "epiuf")} \cr
#' \href{"../doc/epiuf_package.html"}{epiuf_package}
#' 
#' 
#' @param libname Name of the package you want to check for install and load
#'
#' @return library name if loaded else false
#' @export
#' 
#' 
#' @examples
#' loadLib("utils")
#' 

loadLib <- function(libname) {
  if (!require(libname, character.only = TRUE)) {
    cat("Required lib ",libname," is required but not installed")
    rep <- readline("Would you like to install it ? Y/N :")
    if (toupper(rep)=="Y")  {
      install.packages(libname, dependencies = TRUE)
      library(libname, character.only = TRUE)
    }  
    else {
      stop("Some Library have to be installed, check requirements")
    }
  }
  return(as.character(libname))
}

#' epiVersion
#'
#' @return  The version information in short line
#' @export
#'
#' @importFrom utils packageDescription
#' @examples
#' epiVersion()
#' 
epiVersion <- function() {
  result <- packageDescription("epiuf")
  version <- paste("Epiuf version :",result$Version," - ",result$Date," - Built ",result$Built)
  return(version)
}

# END of SCRIPT  --------------------------------------------------------