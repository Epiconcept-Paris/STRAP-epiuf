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
#' prints the list of IDs meeting the specified condition 
#' data dataset to look in 
#' threshold = cutoff number for ID reporting, as number
#' cond = specify variables and outcome to search for eg "Pregnant==1 & Sex==1" or "is.na(Sex)"
#' text = the specified condition to check for 
#' PrintIDCond( df,hosp_id2>=38266 & hosp_id2 < 38500,"hosp_id2",threshold = 50)

#' @param data  A dataframe to look for cond  
#' @param cond  A condition to check  
#' @param text The message to print (if empty the condition is used )
#' @param threshold cutoff number for ID reporting, as number
#' @param column Column name of ID to print
#'
#' @return Message to print as list 
#' @export 
#'
#' @examples
#' df <- data.frame(Id = 1:4 ,
#'                     Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' PrintIDCond(df,Vaccs=="pfizer",threshol=30 ,"Id")

PrintIDCond <- function(data,  cond, text="", threshold , column="id"){
  
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  if  (!column %in% names(data) ){
    column <- names(data)[1]
  }
  if (text == "") {
    text <- as.character(cond)
  }
  
  Records <- subset(data,eval(cond),column)
  
  TextToPrint <- c()
  
  NbCond <- nrow(Records)
  
  if (NbCond != 0){
    
    TextToPrint <- c(TextToPrint,paste0("* ", text,": ", NbCond,"\n"))
    
    if (NbCond > 0 & NbCond < threshold){
      
      listID <-  unlist(Records)
      listID <-  paste(listID, collapse = ", ")
      TextToPrint <- c(TextToPrint, paste0("  + IDs: ", listID, "\n"))
      
    }else if(NbCond >= threshold){
      
      listID <- paste0(threshold, " or more")
      TextToPrint <- c(TextToPrint, paste0("  + IDs: ", listID, "\n"))
      
    }
    
  }
  
  if (length(TextToPrint) != 0){return(TextToPrint)}
  
}

#' Simple count of records satisfying  a conditional expression
#'
#' @param data The dataset where records should be counted 
#' @param cond A logical expression 
#'
#' @return A number of records
#' @export
#'
#' @examples
#' 
#' df <- data.frame(Id = 1:4 ,
#'                     Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' nb <- countIf(df,Vaccs=='pfizer')
#' 
countIf <- function(data,cond) {
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  records <- subset(data,eval(cond))
  r <- nrow(records)
  return(r)
}


# END of SCRIPT  --------------------------------------------------------