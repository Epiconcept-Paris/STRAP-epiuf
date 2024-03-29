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

#' Count Number of Records
#' 
#' Simple count of records satisfying  a conditional expression
#'
#' @param data The dataset where records should be counted 
#' @param cond A logical expression 
#'
#' @return Number of records
#' @export
#'
#' @examples
#' 
#' df <- data.frame(Id = 1:4 ,
#'                  Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' countIf(df)               
#' countIf(df,Vaccs=='pfizer')

countIf <- function(data, cond = NULL) {
  
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
    # records <- data[eval(quote(cond)),]
    
  } else records <- data  
  r <- nrow(records)
  return(r)
}



#' listIf
#' 
#' Simple list all of specified variable for which row data meets specific conditions.
#' Option for removing NAs. Option to have output as list or collapsed string. 
#' First column use as default if no variable specified or variable specified is not within the datset.
#' 
#' @param data The dataset to process
#' @param varname The ID varname which will be printed for each row where condition is not true 
#' @param cond The condition to verify 
#' @param collapse If true then ...
#' @param na.rm  Should missing be included 
#'
#' @return A list of records
#' @export
#'
#' @examples 
#' 
#' df1 <- data.frame(Id = c(1,2,3,4,5,6,7,8,9,10, NA_real_)
#' , A = c("a", "b", "c", "d", "e","a", "b", "c", "d", "e", "b")
#' , B = c("", "dog", "cat", "rabbit", "mole", "dog", "horse", "cat", "", NA_character_, "mouse")
#' )
#' 
#' listIf(data=df1, varname="Id", cond = A=="b", na.rm=TRUE)
#' 

listIf <- function(data, varname=NULL, cond=NULL, collapse=FALSE,na.rm = FALSE){
  
  # set up 2 warnings
  warning1 <- NULL
  warning2 <- NULL
  
  # if no varname provided, default to first line and output warning.
  if  (is.null(varname)){ 
    warning1 <- "No Id provided, first column used"
    varname <- names(data)[1]
  }
  
  # if varname is not in dataset, default to first line and output warning.
  if  (!is.null(varname) & !varname %in% names(data) ){ 
    warning1 <- paste0(varname, " is not in dataset, first column used")
    varname <- names(data)[1]
  }
  
  
  cond <- substitute(cond)
  
  if (!is.null(cond)) {
    if (!typeof(cond)=="language") {cond <- parse(text=cond)}
    
    # Retrieve records matching cond
    # subset will make a lazy eval, in order to work, this one should be done in listIf context 
    # then parent.frame(2)
    records <- subset(data,eval(cond,data,parent.frame(2)))
  } else {
    records <- data
    warning2 <- "No condition provided, all records listed"
  }
  
  
  if (isTRUE(na.rm)){
    listID <- records[!is.na(records[,varname]),varname]
  }else{
    listID <- records[,varname]
  }
  
  if (collapse==TRUE){
    listID <- paste(listID, collapse= ", ")
  }
  
  if(!is.null(warning1)&is.null(warning2)){warning(warning1)}
  if(is.null(warning1)&!is.null(warning2)){warning(warning2)}
  if(!is.null(warning1)&!is.null(warning2)){warning(paste0(warning1, "\n", warning2))}
  
  return(listID)
}



#' printIf
#' 
#' Prints the list of IDs meeting the specified condition
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

#' printIf2
#' 
#' Prints the list of IDs meeting the specified condition
#'  
#' @param data  A dataframe to look for condition  
#' @param cond  A condition to check  
#' @param text The message to print (if empty the condition is used)
#' @param threshold Cutoff number for ID reporting, as number
#' @param varname Column name of ID to print
#' @param na.rm Remove missing ID in the list to print, by default is False
#' @param data_old A dataset where to look for previously checked values 
#'
#' @return Message to print as list 
#' @export 
#'
#' @examples
#' df <- data.frame(ID = 1:4, Vaccs = c("pfizer"," ", "pfizer", "moderna"))
#' printIf(data = df, cond = Vaccs == "pfizer", threshold = 30, text = "Pfizer vaccin", varname = "ID")

printIf2<- function(data,  cond, text = "", threshold = NULL , varname = "id", na.rm = FALSE, data_old=NULL){
  # Set parameters  
  if (text == "") {
    text <- as.character(cond)
  }
  
  if (is.null(threshold)) {threshold <- 50}
  
  TextToPrint <- c()
  
  # retrieve count and list  
  NbCond <- countIf(data=data, cond=cond)
  Records <- listIf(data=data, varname=varname, cond=cond, na.rm=na.rm)
  
  # if count not 0, output in desired format 
  if (NbCond != 0){
    if (NbCond > 0 & NbCond < threshold){ #if number is under threshold
      
      if(!is.null(data_old)){# if a second dataset is provided
        
        Records_old <- listIf(data_old,varname=varname, cond=cond, na.rm=na.rm) # get records list from old set
        
        inOld <- intersect(Records,Records_old) # list those in new that are also in old (ie are repeats)
        inNew <- setdiff(Records, inOld) # list those only in the new
        
        listIDold <-  paste(inOld, collapse = ", ") # collapse repeat IDs
        listIDnew <-  paste(inNew, collapse = ", ") # collapse new IDs
        
        listID <- paste0("[ ", listIDold, " ]  ", listIDnew) # combine to list IDs
        
      } else {# if no second dataset provided, output ids 
        listID <- paste(Records, collapse= ", ")
      }
    }else if (NbCond >= threshold){
      listID <- paste0(threshold, " or more")
    }
    TextToPrint <- c(paste0("* ", text,": ", NbCond,"\n","  + IDs: ", listID, "\n"))
    
  }
  
  
  if (length(TextToPrint) != 0){return(TextToPrint)}
  
}

#' dataCheck
#'
#' @param data The dataset
#' @param data_old A previously checked dataset
#' @param cond The logical condition to test
#' @param text The message to prefix the list in case of error 
#' @param threshold The max number of ID displayed
#' @param varname The ID varname
#'
#' @return a vector
#' @export
#'

dataCheck<- function(data, data_old=NULL, cond, text="", varname=NULL, threshold=NULL){ 
  # Set up the parameters: 
  
  # if condition input not as string, convert to string
  cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  # if no varname provided, default to first line and output warning.
  if  (is.null(varname)){ 
    warning("Warning: no Id provided, first column used")
    varname <- names(data)[1]
  }
  
  # if varname is not in dataset, default to first line and output warning.
  if  (!is.null(varname) & !varname %in% names(data) ){ 
    warning(paste0("Warning: ", varname, " is not in dataset, first column used"))
    varname <- names(data)[1]
  }
  
  #If not threshold is provided, default threshold is 50
  if (is.null(threshold)) {threshold <- 50 } 
  
  # if not text is provided, use condition.
  if (text == "") {
    text <- as.character(cond)
  }
  
  
  # Extract the records  
  # isolate records matching cond in the first dataset
  Records <- subset(data,eval(cond),varname)
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