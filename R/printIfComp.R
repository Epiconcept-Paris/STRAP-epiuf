#
# Project Name : SARI-VEBIS
# Script Name  : printIf2
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : Edits to printIf
# Date created : 10/03/2022
# Author       : JHD from GDS
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Small edit to printIF to set ID designating input to be in CamelCase (ie. Id)
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# line 24 : change "id" to "Id"

# START of SCRIPT  --------------------------------------------------------

#' Title
#'
#' @param data The dataset to check 
#' @param data_old The other dataset which may contain same records
#' @param cond A condition to meet 
#' @param text A message to display if condition is not meet
#' @param threshold Something ..
#' @param varname The ID variaable to display
#'
#' @return
#' @export
#'

printIfComp<- function(data, data_old,  cond, text="", threshold=NULL , varname="id"){ # call in second dataset (that used in the last datacheck)
  
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  if  (!varname %in% names(data) ){
    varname <- names(data)[1]
  }
  if (text == "") {
    text <- as.character(cond)
  }
  if (is.null(threshold)) {threshold <- 50 }
  Records <- subset(data,eval(cond),varname)
  
  TextToPrint <- c()
  
  NbCond <- nrow(Records)
  
  if (NbCond != 0){
    
    TextToPrint <- c(TextToPrint,paste0("* ", text,": ", NbCond,"\n"))
    
    if (NbCond > 0 & NbCond < threshold){
      
      Records_old <- subset(data_old,eval(cond),varname) # get records list from old set
      
      inOld <- intersect(Records, Records_old) # list those in new that are also in old (ie are repeats)
      inNew <- setdiff(Records, inOld) # list those only in the new
      
      listIDold <-  unlist(inOld)
      listIDold <-  paste(listIDold, collapse = ", ") # collapse repeat IDs with no formatting
      
      listIDnew <-  unlist(inNew)
      listIDnew <-  paste(listIDnew, collapse = ", ") # collapse new IDs using bold nomencalture
      
      TextToPrint <- c(TextToPrint, paste0("  + IDs: [ ", listIDold, " ]  ", listIDnew,"\n")) # Print old then new IDs
      
    }else if(NbCond >= threshold){
      
      listID <- paste0(threshold, " or more")
      TextToPrint <- c(TextToPrint, paste0("  + IDs: ", listID, "\n"))
      
    }
    
  }
  
  if (length(TextToPrint) != 0){return(TextToPrint)}
  
}

# END of SCRIPT  --------------------------------------------------------