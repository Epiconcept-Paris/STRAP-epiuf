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

#' Read Data from Various File Types
#'
#' This function reads data from various file types such as CSV, DTA (Stata), REC (EpiInfo), RDA/RDATA, RDS, and Excel files. 
#' It provides options to factorize, convert column names to lowercase, and add labels to the data.
#' If no filename is provided, a file dialog will be opened for the user to select a file.
#'
#' @param filename String specifying the path of the file to read. If empty, opens a file dialog.
#' @param factorise Logical, if TRUE factorizes the data.
#' @param lowercase Logical, if TRUE converts column names to lowercase.
#' @param label Optional label to add to the data.
#' @param sheet Sheet name or index for Excel files (If reading an Excel file)
#' @param verbose Logical, if TRUE prints details about the loaded file.
#' @param ... Additional arguments passed to the underlying read functions 
#' like encoding="latin1".
#
#'
#' @return Returns a data frame of the loaded file, or an error if the file format 
#' is not supported or the file does not exist.
#' @export
#' @importFrom foreign read.epiinfo  
#' @importFrom haven read_dta        
#' @importFrom readxl read_excel     
#'
#' @examples
#' readData("flucases.csv")
#' 
#' 
readData <- function(filename = "", factorise = FALSE, lowercase= FALSE, label = NULL,sheet=NULL, verbose = TRUE,...) {
  # no file name ? choose one using explorer
  if (filename == "") {
    catret("retrieving file tree...please wait.")
    r <- try(filename <- file.choose())
    if (inherits(r, "try-error")) {
      # user have cancelled , stop now
      return(r)
    }
  }
  # try to extract name and extension ...
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
      # haven packages is required
      r <- requireNamespace("haven", quietly = TRUE)
      if (!r) {
        message("Package haven required")
      }
      # Prior to Stata 14, files did not declare a text encoding,
      # haven assumes the encoding is windows-1252,
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
    } else if (ext == "rds") {
      # load an unique object
      dfloaded <- readRDS(filename)
      push.data(fileName(filename),dfloaded)
    } else if (ext == "xls" | ext == "xlsx") {
      # readxl packages is required
      r <- requireNamespace("readxl", quietly = TRUE)
      if (!r) {
        message("Package readxl required")
      }
      dfloaded <- readxl::read_excel(filename,sheet=sheet)
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
      if(isTRUE(verbose)){
        cat("File ", filename, " loaded. \n")
        catret(fileatt[1],
               "Observations of ",
               fileatt[2],
               " variables. Use str(name) for details")
      }
      invisible(as.data.frame(dfloaded))
  
    }
  } else {
    # file doesn't exists ??
    cat("File \"", filename, "\" doesn't exist.\n", sep = "")
    catret("Verify your working directory. Current is", getwd())
    
  }
}



# END of SCRIPT  --------------------------------------------------------