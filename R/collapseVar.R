#
# Project Name : STRAP
# Script Name  : collapseVar
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of collapseVar function
# Date created : 02/03/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------

# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------
#' Title
#'
#' @param data The dataset
#' @param varname Name of variable
#' @param hierachry ordered list to use in collapse
#'
#' @return The variable collapsed 
#' @export
#'

collapseVar <- function(data, varname, hierarchy){
  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
 
  
  # Verify hierarchy input matches variable contents.
  # extract list of unique numbers in variable
  unique <- unique(as.numeric(unlist(regmatches(data[, varname], gregexpr("[[:digit:]]+",data[, varname])))))
  # Identify numbers in variable not included in heirachy
  varNotHier <- setdiff(unique, hierarchy)
  
  if(length(varNotHier)==0) { # if all numbers present...
    # loop through order of codes, searching for matches, and replacing when find.
    for (i in 1:length(hierarchy)) {
      
      data[,varname] <- ifelse(grepl(hierarchy[i], data[,varname]), hierarchy[i], data[,varname] )

    }
    # print success
    catret(varname, ": collapse complete")
    
    return(as.numeric(data[,varname]))
    
  }else if (length(varNotHier)>0) { # if numbers missing...
    # print warning
    catret(varname, ": collapse not run, hierarchy missing ", varNotHier)
  }
  
  return(data[,varname])
  
}

# END of SCRIPT  --------------------------------------------------------

