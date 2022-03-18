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
#' @param data The dataset which contain a multivar 
#' @param mydictionary dico to use (or ordered list ?)
#' @param multivarname Name of multi variable
#'
#' @return
#' @export
#'
#' @examples
collapseVar <- function(data, mydictionary, multivarname){
  
  s_op <- deparse(substitute(multivarname))
  # if multivarname is a variable which contain char, we use content of multivarname
  tryCatch(
    if (is.character(multivarname)) {
      s_op <- multivarname
    }
    , error = function(c) { }
  )
  multivarname <- s_op

# extract recode element from ditionary - currently in c(#, # , #....) format - best?
macroCodeOrder <- subset(mydictionary, generic_name==multivarname)$recode
CodeOrder <- eval(parse(text=macroCodeOrder))

# loop through order of codes, searching for matches, and replacing when find.
for (i in 1:length(CodeOrder)) {
  
  data[,multivarname] <- ifelse(grepl(CodeOrder[i], data[,multivarname]), CodeOrder[i], data[,multivarname] )
  
}
return(data[,multivarname])
}


# END of SCRIPT  --------------------------------------------------------

