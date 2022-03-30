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
# 
# 
# 
#

# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------


#' factorUsing
#'
#' @param data DataSet which contain \code{varname}
#' @param varname The variable to factorise
#' @param dicoCode The list of code 
#' @param dicoLabel The corresponding list of labels
#'
#' @return factored variable
#' @export
#'

factorUsing <- function(data,varname,dicoCode, dicoLabel) {
  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # Verify code and label input coherence and output warnings if length mismatch:
  if(length(dicoCode)>length(dicoLabel)){
    catret("Warning", varname, ": label list shorter than code list, labelling not done")
    return(data[,varname])
    
  }
  else 
    if(length(dicoCode)<length(dicoLabel)){
      catret("Warning", varname, ": code list shorter than label list, labelling not done")
      return(data[,varname])
      
  }
  else
    if(length(dicoCode)==length(dicoLabel)){ 
    
  # Extract all numeric inputs into variable
      varContents <- regmatches(data[, varname], gregexpr("[[:digit:]]+", data[, varname]))
  
  # Create vector to receive successfully labeled variable names
      worked <- c()
          
  # Verify only one number present/row    
      if(any(sapply(varContents, function(x) length(x) > 1)==TRUE)){ # If true print warning
        catret("Warning", varname, ": contains multiple numbers/row, labelling not done")
        return(data[,varname])
        }
      else{ # if false, continue
    
      # Verify variable does not have extra numbers that will go unlabelled
      varUnique <- unique(as.numeric(unlist(varContents)))
      varNotDico <- setdiff(varUnique, dicoCode)
      
      if (length(varNotDico)>0) { # if variable contains numbers not in code, print warning
        catret("Warning",varname, ": input is missing" , varNotDico, ", labelling not done")
        return(data[,varname])
      }
      else
        if (length(varNotDico)==0){ # if all is well, run factoring.
  # make the labelling
  data[,varname] = factor(data[,varname], levels = dicoCode, labels=dicoLabel)
  catret(varname, ": labelling applied sucessfully")
  return(data[,varname])
          }
        }
    }
}


# END of SCRIPT  --------------------------------------------------------


