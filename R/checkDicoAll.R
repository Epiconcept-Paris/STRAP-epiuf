#
# Project Name : STRAP
# Script Name  : checkDico
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of checkDico function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to check codes in source or generic dataset against the data 
# dictionary. Currently output is variable code and dicos code list for all  
# variables where source or generic data has extra code (unless variable is empty).
# For checkbox variables, 




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' checkDicoAll
#'
#' @param data 
#' @param dictionary 
#' @param dicos 
#'
#' @return text outputs
#' @export
#'

checkDicoAll <- function(data, dictionary=NULL, dicos=NULL) {
  
  if(is.null(dictionary)){          # retrieve dictionary from global environment if none specified
    dictionary <- getDictionary()
  }else{
    dictionary <- dictionary
  }
  
  if(is.null(dicos)){          # retrieve dico from global environment if none specified
    dicos <- getDicos()
  }else {
    dicos <- dicos
  }
  
  # Pint header message
  
  catret()
  bold("Variable code and dico code if mismatched:")
  catret()

  dicoVar <- intersect(colnames(df), dictionary$generic_name) # Isolate all varnames that are in the dictionary
  
  for (i in dicoVar){
    diconame <- getDictionaryValue(i, "dico") # output, if dico not present, need standard output - like NA

    if (!is.na(diconame)){
      dicopairs <- getDico(diconame)
      checkDico(data, i, dicopairs$code)
    }else{
      cat()
    }
  }
}


# END of SCRIPT  --------------------------------------------------------

