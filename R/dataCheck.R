#
# Project Name : SARI-VEBIS
# Script Name  : dataCheck
# GitHub repo  : STRAP-EPIUF
# Summary      : core function for extracting data check information
# Date created : 30/05/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------


# START of SCRIPT  --------------------------------------------------------


#' dataCheck
#'
#' @param data 
#' @param data_old 
#' @param cond 
#' @param text 
#' @param threshold 
#' @param varname 
#'
#' @return a vector
#' @export
#'

dataCheck<- function(data, data_old=NULL,  cond, text="", threshold=NULL , varname="id"){ 
  
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  
  if  (!varname %in% names(data) ){ # if varname is not in dataset, default to first line.
    varname <- names(data)[1]
    catret(paste0("Warning:", varname, "is not in dataset, first column used"))
  }
  
  if (is.null(threshold)) {threshold <- 50 } # default threshold is 50

  if (text == "") {
    text <- as.character(cond)
  }
  
  Records <- subset(data,eval(cond),varname) # isolate records matching cond
  NbCond <- nrow(Records) # get count
  
  if (NbCond != 0){ # if have any records meeting cond...
    
    if (NbCond > 0 & NbCond < threshold){ # if number is less than treshold,
      if(!is.null(data_old)){# and a second dataset is provided
        
        Records_old <- subset(data_old,eval(cond),varname) # get records list from old set
        
        inOld <- intersect(Records[, varname],Records_old[,varname]) # list those in new that are also in old (ie are repeats)
        inNew <- setdiff(Records[, varname], inOld) # list those only in the new
        
        listIDold <-  unlist(inOld)
        listIDold <-  paste(listIDold, collapse = ", ") # collapse repeat IDs
        
        listIDnew <-  unlist(inNew)
        listIDnew <-  paste(listIDnew, collapse = ", ") # collapse new IDs
        
        listID <- paste0("[ ", listIDold, " ]  ", listIDnew) # combine to list IDs
        
      } else {# if no second dataset provided, output ids 
        listID <- paste(Records[,varname], collapse= ", ")
      }
      
    }else if(NbCond >= threshold){# if number of ids more than threshold, print message
      
      listID <- paste0(threshold, " or more")
    }
    return(c(text, NbCond,listID))
  }else{return(NULL)}
}

# END of SCRIPT  --------------------------------------------------------