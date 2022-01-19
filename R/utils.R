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


#' camel
#' To transform snake_case into CamelCase 
#' 
#' @param x a string or a srinf list or a vector of strings 
#'
#' @return a CamelCase value or list 
#' @export  
#'
#' @examples
#' test <- "snake_case"
#' camel(test)
#' 
camel <- function(x){         
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "_"), function(x) paste(capit(x), collapse=""))
}


#' catret
#'    cat ret is a wrapper for cat(...,"newline")
#' @param ... list of values to concatenate for console output
#'
#' @return  nothing
#' @export
#'
#' @examples
#' catret("test")
#' 
catret  <- function(...) {
  cat(...,"\n")
}

# count number of specific char into a text using reg expr
charCount <- function(pattern, stosearch) {
  lengths(regmatches(stosearch, gregexpr(pattern, stosearch)))
  # length(attr(gregexpr(pattern,stosearch)[[1]],
  #            "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}


ask <- function(message,answers) {
  r <- ""
  while(r=="" ){
    n <- readline(message)
    if(!is.na(match(n,as.vector(answers)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    break
  }
}

ok <- function() {
  ask("Do you confirm?", c("Yes", "Y", "y") )
}

bold <- function(...) {
  cat("\033[1m",...,sep="")
}

italic <- function(...) {
  cat("\033[3m",...,sep="")
}

red <- function(...) {
  cat("\033[31m",...,sep="")
}

normal <- function(...) {
  cat("\033[0m",...,sep="")
}



# END of SCRIPT  --------------------------------------------------------