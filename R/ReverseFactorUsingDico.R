#
# Project Name : STRAP
# Script Name  : factorUsingDico
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of factorUsingDico function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to factorise and label coded variables based off the data dictionary 
# 
# 
#

# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' reverseFactorUsingDico
#'
#' @param data A dateset to process
#' @param dictionary A dictionary epiuf structure
#' @param dicos A dicos epiuf structure 
#'
#' @return The dataset modified 
#' @export
#'

reverseFactorUsingDico <- function(data, dictionary=NULL, dicos=NULL) {
  
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

dicoVar <- intersect(colnames(data), dictionary$generic_name) # Isolate all varnames that are in the dictionary
  
  for (i in dicoVar){
    diconame <- getDictionaryValue(i, "dico") # output, if dico not present, need standard output - like NA
    
  if (!is.na(diconame)){
    dicopairs <- getDico(diconame)
    
    data[,i] <- factorUsing(data,i, dicopairs$label, dicopairs$code)
  }else{
    data[,i] <- data[,i]
  }
  }
  return(data)
}

# tab(CovvaccanyFirstdose)
# df1 <- factorUsingDico(df)
# tab(CovvaccanyFirstdose, data=df1)
# df2 <- ReverseFactorUsingDico(df1)
# tab(CovvaccanyFirstdose, data=df2)

# END of SCRIPT  --------------------------------------------------------
