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
# Function aims to take checkbox variables that are not expanded, and retain only
# one input / patient based off hirearchy of input. Hierarchy could be derrived from dico codes
# set in specific order. Idea is that all checkboxes can be given action group
# tags depending on if they are to be expanded or collapse.

# collapseVar(data, varname, code hirearchy) (take hirearchy from dicolist - put in desired order manually) - is that dangerous????
#   - get separated string list for each input. 
# - if length is 1 - leave
# - if length is >1 - assess
# - retain highest value from the order of preference


# For expandVar, can put the list of info into the recode box - need to amend this code
# For collapseVar, put the hireachry list into the recode box - how do with (all vs x?)



# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------
#' collapseVarAll
#'
#' @param data Dataset to be processed
#' @param action A Dictionary action list structure 
#'
#' @return The dataset with collapsed variable
#' @export
#'

collapseVarAll <- function(data, action=NULL){
  
  if(is.null(action)){
    getDictionaryActions()
  }
  
  collapseActionGroup <- getActionGroup("checkboxcollapse")$variable # get list of all variables taged for an action

  collapseVars <- intersect(colnames(data), collapseActionGroup) # Isolate all varnames associated with collapse action

# loop through order of codes, searching for matches, and replacing when find.
for (i in collapseVars) {
  
  CodeOrder <- eval(parse(text = getVarActionParameters(i, "checkboxcollapse")))         # get collapse code hierachy list for each variable

  data[,i] <- collapseVar(data, i, CodeOrder)

}

return(data)
}


# END of SCRIPT  --------------------------------------------------------

