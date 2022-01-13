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


#' expandMacro
#'
#' @param x A string containing a macro 
#'
#' @return  The string with macro processed and repalced by value
#' @export
#' @importFrom R.utils gstring
#'
#' @examples
#' varMacro <- "few word"
#' expandMacro("text with ${varMacro} inside")
#' 
expandMacro <- function(x) {
  R.utils::gstring(x) 
}


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