#
# Project Name : STRAP
# Script Name  : expandVarAll
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of expandVarAll function
# Date created : 02/06/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------


# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------


#' Expand all variables in a dataset based on predefined actions
#'
#' This function automates the expansion of variables in a dataset according to
#' a set of predefined actions. If no action is specified, it retrieves the
#' dictionary of actions. It identifies all variables within the dataset that
#' have been tagged for expansion and applies the expansion action to each.
#'
#' @param data A data frame containing the variables to be expanded.
#' @param action Optional; an action or set of actions to apply for expanding the variables.
#'   If `NULL`, the function will attempt to retrieve the dictionary of actions.
#'
#' @return Returns the modified data frame with the specified variables expanded.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # Assuming 'data' is a data frame and 'action' is defined or NULL to use default actions
#' expandVarAll(data = data)
#' }
expandVarAll <- function(data, action=NULL){
  
  
  if(is.null(action)){
    getDictionaryActions()
  }
  
  expandActionGroup <- getActionGroup("expand")$variable # get list of all variables tagged for an action
  
  expandVars <- intersect(colnames(data), expandActionGroup) # Isolate all varnames associated with collapse action
  
  # loop through order of codes, searching for matches, and replacing when find.
  for (i in expandVars) {
    
    InfoSource <- eval(parse(text = getVarActionParameters(i, "expand")))   # get list input for expanding variables
    
    data <- expandVar(data = data, varname = i, valueslist = InfoSource)
    
  }
  
  return(data)
}


# END of SCRIPT  --------------------------------------------------------

