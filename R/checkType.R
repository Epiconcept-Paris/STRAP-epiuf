#
# Project Name : STRAP
# Script Name  : checkType
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of checkType function
# Date created : 31/05/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------





# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------



#' 
#' @title checkType
#'
#' @description Gives feedback on whether a variable matches the type specified. 
#' 
#' @param data The dataset (data.frame).
#' @param varname The variable to test. 
#' @param type The expected type specified in "". 
#'
#' @return report
#' @export
#'
#' @examples
#' 
#' set.seed(123)  # for reproducibility
#' 
#' # Create an example data frame
#' df <- data.frame(
#'   id = 1:10,
#' name = sample(c("Alice", "Bob", "Charlie", "Diana", "Eve"), 10, replace = TRUE),
#' age = sample(18:60, 10, replace = TRUE),
#' date1 = sample(seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "day"), 10)
#' )
#' 
#' # Check if the variable 'age' is an integer 
#' checkType(data = df,
#'          varname = age,
#'          type = c('integer'))
#' 
checkType <- function(data, varname, type=c("character", "date", "factor", "numeric")) {
  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  classVar <- class(data[, varname])
  
  if(classVar==type) {
    catret(varname, "matches type", type)
  }else{
    catret( "Dictionary specifies:", type, ",", varname, "is:",classVar)
  }
}




# END of SCRIPT  --------------------------------------------------------

