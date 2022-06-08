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



#' checkType
#' Gives feedback on whether a variable matches the type specified. 
#' 
#' @param data The dataset
#' @param varname The var 
#' @param type The expeted type
#'
#' @return report
#' @export
#'
#' @examples
#' \dontrun{
#'    checkType(data,var)
#' }
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

