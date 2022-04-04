#
# Project Name : STRAP
# Script Name  : tab
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of tab function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function to easily output tabs and cross tabs of variables - wrapper for 
# the table() function. Requires dply. name of dataset is by default df, but
# can be changed.
# 




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------


# Function: 
#' tab
#'
#' @param ... 
#' @param includeNA 
#' @param data 
#'
#' @return cross table
#' @export
#'
#' @examples
tab <- function( ..., includeNA="ifany", data=df) {
  # first we catch the ... parameters
  listvar <- as.list(match.call())
  # we remove the first one which is the function name
  listvar[1] <- NULL
  # and we remove all named parameters from the list
  namedarg <- pmatch(c("data", "includeNA"),names(listvar), nomatch = 0)
  if (length(namedarg) > 0 ) {listvar[namedarg] <- NULL }
  
  # We verify that parameters are language and if it is "test" then we parse as language
  if (length(listvar)>0) {
    for (i in 1:length(listvar)) {
      if (!typeof(listvar[i])=="language") {listvar[i] <- parse(text=listvar[i])}
    }
  }  
  
  # create list with first vector to receive names
  varname <- as.character(listvar[1])
  
  if (length(listvar)>1) {
    # extract each name in turn and add to list
    for (i in 2:length(listvar)){
      varname <- c(varname, as.character(listvar[i]))
    }
  } 
  
  # use list of colunms to make table
  table(data[, varname], exclude=includeNA) 
  
}


# END of SCRIPT  --------------------------------------------------------
