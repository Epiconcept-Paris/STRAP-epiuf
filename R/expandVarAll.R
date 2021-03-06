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


#' expandVarAll
#'
#' @param data dataframe to be processed, the dico must be loaded 
#'
#' @return data.frame
#' @export
#'
expandVarAll <- function(data){
  
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

