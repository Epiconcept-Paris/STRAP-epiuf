#
# Project Name : STRAP
# Script Name  : collapseVar
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of collapseVar function
# Date created : 02/03/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :



# For expandVar, can put the list of info into the recode box - need to amend this code
# For collapseVar, put the hireachry list into the recode box - how do with (all vs x?)



# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' collapsVar
#' Function aims to take checkbox variables that are not expanded, and retain only
#' one input / patient based off hirearchy of input. Hierarchy could be derrived from dico codes
#' set in specific order. Idea is that all checkboxes can be given action group
#' tags depending on if they are to be expanded or collapse.
#' collapseVar(data, varname, code hirearchy) (take hirearchy from dicolist - put in desired order manually) - is that dangerous????
#' - get separated string list for each input.
#' - if length is 1 - leave
#' - if length is >1 - assess
#' - retain highest value from the order of preference
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
    
    return(as.numeric(data[,varname]))
    
  }else if (length(varNotHier)>0) { # if numbers missing...
    # print warning
    catret(varname, ": collapse not run, hierarchy missing ", varNotHier)
  }
  
  return(data[,varname])
  
}


# END of SCRIPT  --------------------------------------------------------

