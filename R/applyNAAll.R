#
# Project Name : STRAP
# Script Name  : applyNa
# GitHub repo  : STRAP-Common-Tasks
# Summary      : construction fo the applyNa function
# Date created : 15/3/22
# Author       : Jenny
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# A) searches through character variables for common abbreviations of unknown
# and replace with NA
# B) uses dico and dictionary to identify factor coded unknowns and replace with NA.
# Outputs data frame


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


#' applyNAAll
#'
#' @param data The dataset where to applyNA for each variable using the dictionnary
#' @param dictionary an optional dictionary (epiuf strucure see \link{getDictionary})
#' @param join Should default NA value added to those from dictionary 
#'
#' @return data.frame and printed output
#' @export
#'
#' 
#'
applyNAAll <- function(data, dictionary=NULL, join=TRUE){
  
  if(is.null(dictionary)){          # retrieve dictionary from global environment if none specified
    ds <- getDictionary()
  }else{
    ds <- dictionary
  }
  
  dicoVar <- intersect(colnames(data), ds$generic_name) # Isolate all varnames that are in the dictionary (flag no warnings for those not in dictionary or vice versa here, that is for other functions!)
  
  for (i in dicoVar){
    unk <- getDictionaryValue(i, "unknowns")      # retrieve list of coded unknowns if present. (NA if none given)

    if (!is.na(unk)){
      data[,i] <- applyNA(data, i, unk, join)
    }
    else if (is.na(unk)){
      data[,i] <- applyNA(data, i)  
    }
  }
  return(data)
}


# END of SCRIPT  --------------------------------------------------------

