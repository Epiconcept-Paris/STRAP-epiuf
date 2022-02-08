#
# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------
# PrintIDCond : prints the list of IDs meeting the specified condition 
## data = "name of dataset to look in" eg "df"
## threshold = cutoff number for ID reporting, as number
## Cond = specify variables and outcome to search for eg "Pregnant==1 & Sex==1" or "is.na(Sex)"
## NB: the "" are important
## PrintIDCond( df,50,hosp_id2>=38266 & hosp_id2 < 38500,"hosp_id2")


#' PrintIDCond
#'
#' @param data  A dataframe to look for cond  
#' @param threshold Max number of ID to print 
#' @param cond  A condition 
#' @param column Column nameto print
#'
#' @return nothing but print using cat
#' @export 
#'
#' @examples
#' data <- data.frame(Id = 1:4 , 
#'                     Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' PrintIDCond(data,50,Vaccs=="pfizer","Id")
#'                      
PrintIDCond <- function(data, threshold, cond, column="IdData"){
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  Records <- subset(data,eval(cond),column)
  
  NbCond <- nrow(Records)
  if (NbCond > 0 & NbCond < threshold){
    listID <-   unlist(Records)
    listID <-  paste(listID, collapse = ", ")
  }else if(NbCond >= threshold){
    listID <- paste0(threshold, " or more IDs")
  }else if(NbCond==0) {
    listID<- "none"
  }
  cat(paste0("+ IDs: ", listID))
}


# END of SCRIPT  --------------------------------------------------------