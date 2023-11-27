#
# Project Name : STRAP
# Script Name  : setTypeAll
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of setTypeAll function
# Date created : 27/11/23
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' setTypeAll
#' Uses dictionary to define the class for each variable.
#' 
#' @param data The dataset
#' @param dictionary The epiuf dictionary
#'
#' @return data frame
#' @export
#'
#' @examples
#'  \dontrun{
#'    setTypeAll(data)
#'    }
#' 
setTypeAll <- function(data, dictionary=NULL){
  
  if(is.null(dictionary)){          # retrieve dictionary from global environment if none specified
    ds <- getDictionary()
  }else{
    ds <- dictionary
  }
  
  dicoVar <- intersect(colnames(data), ds$generic_name) # Isolate all varnames that are in the dictionary
  
  ## This is the slow looping method also used in applyNAAll 
  ## - thus any improvements to applyNAAll will need to be put here too!
  
  for (i in dicoVar){
    tp <- getDictionaryValue(i, "type") # retrieve list of type if present. (NA if none given)
    
    if (!is.na(tp)){ # if type provided, then set variable to be this type
      data[,i] <- setType(data, i, tp)
    }
  }
  # return the dataset
  return(data)
}



# END of SCRIPT  --------------------------------------------------------

