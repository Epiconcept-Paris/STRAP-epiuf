#
# Project Name : STRAP
# Script Name  : checkDico
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of checkDico function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------





# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------


#' checkDico
#'
#' @param data 
#' @param varname 
#' @param dicoCode 
#'
#' @return text outputs
#' @export
#'

checkDico <- function(data, varname, dicoCode) {
  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  varUnique <- unique(as.numeric(unlist(regmatches(data[, varname], gregexpr("[[:digit:]]+", data[, varname])))))
  varNotDico <- setdiff(varUnique, dicoCode)
  
  if (length(varNotDico)>0) {
    
    catret(varname, "contains:" , varUnique, ", Code is:", dicoCode)
    catret()
  }
}




# END of SCRIPT  --------------------------------------------------------

