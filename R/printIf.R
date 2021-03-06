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

#' 
#' prints the list of IDs meeting the specified condition
#'  
#' @param data  A dataframe to look for condition  
#' @param cond  A condition to check  
#' @param text The message to print (if empty the condition is used)
#' @param threshold Cutoff number for ID reporting, as number
#' @param varname Column name of ID to print
#' @param na.rm Remove missing ID in the list to print, by default is False
#'
#' @return Message to print as list 
#' @export 
#'
#' @examples
#' df <- data.frame(ID = 1:4, Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' printIf(data = df, cond = Vaccs == "pfizer", threshold = 30, text = "Pfizer vaccin", varname = "ID")

printIf<- function(data,  cond, text = "", threshold = NULL , varname = "id", na.rm = FALSE){
  
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  if (!varname %in% names(data)){
    varname <- names(data)[1]
  }
  if (text == "") {
    text <- as.character(cond)
  }
  if (is.null(threshold)) {threshold <- 50}
  Records <- subset(data,eval(cond),varname)
  
  TextToPrint <- c()
  
  NbCond <- nrow(Records)
  
  if (NbCond != 0){
    
    TextToPrint <- c(TextToPrint,paste0("* ", text,": ", NbCond,"\n"))
    
    if (NbCond > 0 & NbCond < threshold){
      
      if (isTRUE(na.rm)){
        listID <- unlist(na.omit(Records))
      }else{
        listID <-  unlist(Records)
      }
      
      listID <-  paste(listID, collapse = ", ")
      
      if (listID == ""){
        TextToPrint <- TextToPrint
      } else{
        TextToPrint <- c(TextToPrint, paste0("  + IDs: ", listID, "\n"))
      }
      
      
    }else if (NbCond >= threshold){
      
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
#' nb <- countIf(df)
countIf <- function(data,cond=NULL) {

  cond <- substitute(cond)
  
  if (!is.null(cond)) {
    
    if (!typeof(cond)=="language") {cond <- parse(text=cond)}
    
    # base version 
    # records <-  eval(cond,data,parent.frame())
    # records <- records & !is.na(records)
    # records <- data[records,]

    # subset will make a lazy eval, in order to work, this one should be done in countIf context 
    # then parent.frame(2)
    records <- subset(data,eval(cond,data,parent.frame(2)))
  } else records <- data  
  r <- nrow(records)
  return(r)
}


# END of SCRIPT  --------------------------------------------------------