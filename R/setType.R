#
# Project Name : STRAP
# Script Name  : setType
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of setType function
# Date created : 27/11/23
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------





# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------



#' setType
#' Transforms a column in a dataset to the defined class (character, date, factor,
#' numeric)
#' 
#' @param data The dataset
#' @param varname The var 
#' @param type The desired type
#'
#' @return variable
#' @export
#'
#' @examples
#' \dontrun{
#'    setType(data,var, "character")
#' }
#' 
setType <- function(data, varname, type=c("character", "date", "factor", "numeric")) {
  ## Are these all the classes we may encounter / require? ----
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # set variable to assigned type
if(type=="character"){
  data[,varname] <- as.character(data[,varname])
}
  if(type=="date"){ 
    data[,varname] <- as.Date(data[,varname])
  } ## Do we want to specify the format of the date?
  
  if(type=="factor"|type=="numeric"){ 
    data[,varname] <- as.numeric(data[,varname])
  } ## Will factor variables always be numeric?
  
  ## Need to return a warning for number of inputs set to missing
  
  return(data[,varname])
  ## Return variable to follow format of other functions in epiuf
}




# END of SCRIPT  --------------------------------------------------------

