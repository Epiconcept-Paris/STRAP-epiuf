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



#' Title
#'
#' @param filename Name of the file to read/import
#' @param factorise Should character value be factorised, default = FALSE
#' @param lowercase Should variable names appear in lower case or as is , default to lowercase
#' @param label Not used currently
#' @param sheet Name of the excel sheet to be read
#' @param ... Other parameters passed to the base function
#'
#' @return The dataset read from file
#' @export
#' @importFrom foreign read.epiinfo
#' @importFrom haven read_dta
#' @importFrom readxl read_excel
#'
#' @examples
#' readData("flucases.csv")
#' 
#' 
readData <- function(filename = "", factorise = FALSE, lowercase= FALSE, label = NULL,sheet=NULL,...) {
  # no file ? choose one
  if (filename == "") {
    catret("retrieving file tree...please wait.")
    r <- try(filename <- file.choose())
    if (inherits(r, "try-error")) {
      # user have cancelled , stop now
      return(r)
    }
  }
  # try to extract name...
  ext <- tolower(fileExt(filename))
  name <- fileName(filename)
  if (file.exists(filename)) {
    # file exists.. let's go
    if (ext == "csv") {
      # look at the content
      # count and identify separator
      testline <- readLines(filename , n = 2)
      comma1 <- charCount(",", testline[1])
      semicol1 <- charCount(";", testline[1])
      if (comma1 > 0) {
        dfloaded <- utils::read.csv(filename,as.is = !factorise,...)
      } else if (semicol1  > 0) {
        dfloaded <- utils::read.csv2(filename,as.is = !factorise,...)
      } else {
        red("Separator not identified in :")
        catret("")
        catret(testline[[1]])
        catret(testline[[2]])
        cat("read.csv used, verify result")
        dfloaded <- utils::read.csv(filename,as.is = !factorise,...)
      }
    } else  if (ext == "dta") {
      # foreign packages is required
      r <- requireNamespace("haven", quietly = TRUE)
      if (!r) {
        message("Package haven required")
      }
  # Prior to Stata 14, files did not declare a text encoding,haven assumes the encoding is windows-1252,
  # Stata Mac and Linux  use a different default encoding, "latin1". 
      dfloaded <- haven::read_dta(filename)    # encoding = "latin1"
    } else if (ext == "rec") {
      # foreign packages is required
      r <- requireNamespace("foreign", quietly = TRUE)     
      if (!r) {
        message("Package foreign required")
      }
      dfloaded <- foreign::read.epiinfo(filename)
    } else if (ext == "rda" | ext == "rdata" ) {
      # load return name and load content into selected env
      dfloaded <- load(filename)
      dfloaded <- get(dfloaded)
    } else if (ext == "xls" | ext == "xlsx") {
      # foreign packages is required
      r <- requireNamespace("readxl", quietly = TRUE)
      if (!r) {
        message("Package readxl required")
      }
      dfloaded <- read_excel(filename,sheet=sheet)
    } else {
      catret("Extension '", ext, "'not found")
    }
    if (!missing(label)) {
      attr(dfloaded, "label") <- label
    }
    if (is.data.frame(dfloaded)) {
      fileatt <- dim(dfloaded)
      if (lowercase) {
        names(dfloaded)<-tolower(names(dfloaded))
      }
      cat("File ", filename, " loaded. \n")
      catret(fileatt[1],
          "Observations of ",
          fileatt[2],
          " variables. Use str(name) for details")
      invisible(as.data.frame(dfloaded) )
    }
  } else {
    # file doens't exists ??
    cat("File \"", filename, "\" doesn't exist.\n", sep = "")
    catret("Verify your working directory. Current is", getwd())
    
  }
}

  

# END of SCRIPT  --------------------------------------------------------