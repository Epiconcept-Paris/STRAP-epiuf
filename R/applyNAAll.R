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


#' applyNA
#'
#' @param data 
#' @param dictionary (optional)
#'
#' @return data.frame and printed output
#' @export
#'
#' @examples df <- applyNAAll(df)
#' 
#'
applyNAAll <- function(data, dictionary=NULL){
  
  if(is.null(dictionary)){          # retrieve dictionary from global environment if none specified
    dictionary <- getDictionary()
  }else{
    dictionary <- dictionary
  }
  
  dicoVar <- intersect(colnames(data), dictionary$generic_name) # Isolate all varnames that are in the dictionary (flag no warnings for those not in dictionary or vice versa here, that is for other functions!)
  
  for (i in dicoVar){
    unk <- getDictionaryValue(i, "unknowns")      # retrieve list of coded unknowns if present. (NA if none given)

    if (!is.na(unk)){
      data[,i] <- applyNA(data, i, unk, join=TRUE)
    }
    else if (is.na(unk)){
      data[,i] <- applyNA(data, i)  
    }
  }
  return(data)
}


# END of SCRIPT  --------------------------------------------------------

