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
setType <- function(data, varname, type=c("character", "date", "factor", "numeric"), dateformat = NULL) {
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
  
  # Extract number of missing inputs
  orig_missing <- nrow(data[is.na(data[,varname]),])
  

  withCallingHandlers({ # Capture warning to add count of those changed to NA when this warning is given
  # set variable to assigned type
if(type=="character"){
  data[,varname] <- as.character(data[,varname])
}
  if(type=="date"&is.null(dateformat)){ 
    data[,varname] <- as.Date(data[,varname])
  } 
    
  if(type=="date"&!is.null(dateformat)){ 
      data[,varname] <- as.Date(data[,varname], format = dateformat)
    } 
  
  if(type=="factor"|type=="numeric"){ 
    data[,varname] <- as.numeric(data[,varname])
  } ## Will factor variables always be numeric?
  
  ## Calculate new number of missing
    new_missing <- nrow(data[is.na(data[,varname]),]) - orig_missing 
    
    if(type=="date" & new_missing>0){ # as.Date does not give this warning, thus must force
      warning("NAs introduced by coercion")
    }
    
  },warning = function(w, num_na = new_missing){
    if(conditionMessage(w)=="NAs introduced by coercion"){
    message("Warning: ",varname," ",num_na , " ",conditionMessage(w))
    }else{message("Warning: ",varname," ",conditionMessage(w))} # if warning is of a different sort, then display
    invokeRestart("muffleWarning")
  })
  
  
  return(data[,varname])
  ## Return variable to follow format of other functions in epiuf
}




# END of SCRIPT  --------------------------------------------------------

