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

#' Collapse a variable in a dataset according to a hierarchy
#'
#' This function collapses a variable within a dataset based on a given hierarchy.
#' It replaces values in the variable by matching them with elements in the hierarchy.
#' If the variable contains elements not in the hierarchy, a warning is issued.
#'
#' @param data A data frame containing the variable to be collapsed.
#' @param varname The name of the variable to collapse. This can be a character string
#'   denoting the variable name or the variable itself.
#' @param hierarchy A numeric vector representing the hierarchy in which to collapse the variable.
#'   Values in `varname` will be replaced according to the order of this hierarchy.
#'
#' @return Returns the modified variable as a numeric vector if the collapse is complete.
#'   If the collapse is not run due to missing elements in the hierarchy, it returns the
#'   original variable.
#'
#' @examples
#' \dontrun{
#' # Assuming 'data' is a data frame and 'hierarchy' is a numeric vector
#' collapseVar(data = data, varname = "someVar", hierarchy = c(1, 2, 3))
#' }
#'
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
   unique <- unique(data[,varname])
   unique <- unique(as.numeric(unlist(regmatches(unique, gregexpr("[[:digit:]]+",unique)))))
  # unique <- unique(as.numeric(unlist(regmatches(data[, varname], gregexpr("[[:digit:]]+",data[, varname])))))
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

