#
# Project Name : STRAP
# Script Name  : checkTypeAll
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of checkTypeAll function
# Date created : 31/05/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' checkTypeAll
#' Uses dictionary to compare class of each variable to type listed. 
#' A specific class of interest can be specified with the flag parameter
#' 
#' @param data The dataset
#' @param dictionary The epiuf dictionary
#' @param flag A flag 
#'
#' @return report
#' @export
#'
#' @examples
#'  \dontrun{
#'    checkTypeAll(data)
#'    }
#' 
checkTypeAll <- function(data, dictionary=NULL, flag=NULL) {
  
  if(is.null(dictionary)){          # retrieve dictionary from global environment if none specified
    dictionary <- getDictionary()
  }else{
    dictionary <- dictionary
  }
  
  # Pint header message
  
  catret()
  bold("Comparison of variable type in dataset and in the dictionary:")
  catret()
  
  typeVar <- intersect(colnames(data), dictionary$generic_name) # Isolate all varnames that are in the dictionary
  
  if(is.null(flag)){ # if flag is null, print all comparisons
  for (i in typeVar){
    typename <- getDictionaryValue(i, "type") # output, if type not present, need standard output - like NA
    
    if (!is.na(typename)){
      checkType(data, i, typename)
    }else{
      cat()
    }
  }
  }else{
    for (i in typeVar){
      typename <- getDictionaryValue(i, "type") # output, if type not present, need standard output - like NA
      vartype <- class(data[,i])
      
      if (!is.na(typename)&(typename==flag|vartype==flag)){ # for only the type of variable flagged show output
        checkType(data, i, typename)
      }else{
        cat() 
      }
      }
  }
}



# END of SCRIPT  --------------------------------------------------------

