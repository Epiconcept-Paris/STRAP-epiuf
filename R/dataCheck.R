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

  s_cond <- substitute(cond)
  if (!is.null(s_cond)) {
      if (typeof(s_cond)=="symbol") {
        if (! s_cond == "cond") {
          # do something for a variable ? 
        }    
      } else if (! typeof(s_cond)=="language") {
        cond <- parse(text=s_cond)
      } else cond = s_cond
    # base version 
    # records <-  eval(cond,data,parent.frame())
    # records <- records & !is.na(records)
    # records <- data[records,]
    
    # subset will make a lazy eval, in order to work, this one should be done in countIf context 
    # then parent.frame(2)
    records <- subset(data,eval(cond,data,parent.frame(2)))
    #records <- data[eval(quote(cond)),]
  } else records <- data  
  r <- nrow(records)
  return(r)
}

#' listIf
#' Simple list all of specified variable for which row data meets specific conditions.
#' Option for removing NAs. Option to have output as list or collapsed string. 
#' First column use as default if no variable specified or variable specified is not within the datset.
#' 
#' @param data 
#' @param varname 
#' @param cond 
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

listIf <- function(data, varname=NULL, cond=NULL,  collapse=FALSE, na.rm = FALSE, data_old=NULL){

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

  # enable cond to be used in subset function
  s_cond <- substitute(cond)

  if (!is.null(s_cond)) { # if cond has been given
    if (typeof(s_cond)=="symbol") {
      if (! s_cond == "cond") {
        # do something for a variable ? 
      } 
    } else if (!typeof(s_cond)=="language") {
      cond <- parse(text=s_cond)
    }  else cond = s_cond

  # Retrieve records matching cond
    # subset will make a lazy eval, in order to work, this one should be done in listIf context 
    # then parent.frame(2)
    Records <- subset(data,eval(cond,data,parent.frame(2)), varname)
  } else {
    Records <- subset(data,TRUE,varname)
    warning2 <- "No condition provided, all records listed"
  }

  if (isTRUE(na.rm)){
    Records <- Records[!is.na(Records[,varname]),varname]
  }
  
  listID <- Records
  
  if(collapse==TRUE){ # if defined, collapse to string output
    listID <- paste(listID, collapse= ", ")  
  }
  
    if(!is.null(data_old)){ # if a second dataset is provided

      if (!is.null(s_cond)) { # if cond has been given
        # Retrieve records matching cond
        # subset will make a lazy eval, in order to work, this one should be done in listIf context 
        # then parent.frame(2)        
         Records_old <- subset(data_old,eval(cond,data,parent.frame(2)),varname) # get records list from old set
      } else {
        Records_old <- data_old[,varname]
      }
      
    if (isTRUE(na.rm)){ # remove NA if na.rm is TRUE
      Records_old <- Records_old[!is.na(Records_old[,varname]),varname]
    }
    
    listIDold <- intersect(Records[, varname],Records_old[,varname]) # list those in new that are also in old (ie are repeats)
    listIDnew <- setdiff(Records[, varname], listIDold) # list those only in the new
    
    listID <- list(repeats = listIDold, new = listIDnew)
    
    if(collapse==TRUE){
      listIDold <-  paste(listIDold, collapse = ", ") # collapse repeat IDs
      
      listIDnew <-  paste(listIDnew, collapse = ", ") # collapse new IDs
      
      listID <- paste0("[ ", listIDold, " ]  ", listIDnew) # combine to list IDs
    }
    }

  if(!is.null(warning1)&is.null(warning2)){warning(warning1)}
  if(is.null(warning1)&!is.null(warning2)){warning(warning2)}
  if(!is.null(warning1)&!is.null(warning2)){warning(paste0(warning1, "\n", warning2))}

  return(listID)
}


#' printIf2
#' Prints the caption, count and list of IDs meeting the specified condition in 
#' rmd bullet point format. Aim is to replace printIf.
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

printIf2<- function(data,  cond, text = "", threshold = NULL , varname = "id", na.rm = FALSE, data_old=NULL){
# Set parameters  
  if (text == "") {
    text <- as.character(cond)
  }
  
  if (is.null(threshold)) {threshold <- 50}

  TextToPrint <- c()
  
  s_cond <- (substitute(cond))
  
  if (!typeof(s_cond)=="language") {
    cond <- parse(text=s_cond)
  } else cond = s_cond
  
  
# retrieve count and list  
  NbCond <- countIf(data=data, cond=cond)
 
# if count not 0, output in desired format 
  if (NbCond != 0){
    if (NbCond > 0 & NbCond < threshold){ #if number is under threshold
      listID <- listIf(data=data, varname=varname, cond=cond, collapse=TRUE, na.rm=na.rm, data_old=data_old)
    }else if (NbCond >= threshold){
      listID <- paste0(threshold, " or more")
    }
    TextToPrint <- c(paste0("* ", text,": ", NbCond,"\n","  + IDs: ", listID, "\n"))
  }
  
  
  if (length(TextToPrint) != 0){return(TextToPrint)}
  
}


#' printKableIf
#' Takes any number of lists of caption-condition pairs, and vector of headers to match each list.
#' Outputs a table sectioned / list with header.
#' 
#' @param data 
#' @param parameters 
#' @param header 
#' @param threshold 
#' @param varname 
#' @param na.rm 
#' @param data_old 
#'
#' @return table
#' @export
#'
#' @examples
#' parameter <- list(
#' "!is.na(SariWho) & SariWho==0 & ((!is.na(SympFever)&SympFever==0) | (!is.na(SympCough) & SympCough==0))" = "**Not meeting SARI case definition due to not having fever or cough symptoms**"
#', "!is.na(SariWho)&SariWho==0&is.na(Dischargedate)&!is.na(Outcome)&Outcome==2" = "**Not meeting SARI case definition due to missing hospital discharge date when outcome is discharged**"
#', "!is.na(SariWho)&SariWho==0&is.na(Deathdate)&!is.na(Outcome)&Outcome==1" = "**Not meeting SARI case definition due to missing hospital death date when outcome is deceased**"
#', "!is.na(SariWho)&SariWho==0&is.na(Transferdate)&!is.na(Outcome)&Outcome==4" = "**Not meeting SARI case definition due to missing hospital transfer date when outcome is transferred.**"
#')
printKableIf <- function(data,  parameters, header="Data Check", threshold = NULL , varname = "id", na.rm = FALSE, data_old=NULL){
   
  #If not threshold is provided, default threshold is 50
  if (is.null(threshold)) {threshold <- 50 } 

  datacheck <- data.frame("Check"="", "Number"="", "Id" = "")#create empty dataframe 
  
  for (i in 1:length(parameters)){# loop through all condition text pairs in the list
    
    cond <- parse(text=names(parameters[i])) # retrieve condition from list
    text <-as.character(parameters[i]) # retrieve text caption from list
    
    NbCond <- countIf(data=data, cond=cond)
    listID <- NULL
    
    if (NbCond != 0){
      if (NbCond > 0 & NbCond < threshold){ #if number is under threshold
        listID <- listIf(data=data, varname=varname, cond=cond, collapse=TRUE, na.rm=na.rm, data_old=data_old)
      }else if (NbCond >= threshold){
        listID <- paste0(threshold, " or more")
      }
    if (!is.null(listID)){
      RowToPrint <- c(text, NbCond,listID) # assemble row for each check
      datacheck <- rbind(datacheck, RowToPrint) # append to data frame
    }
    }
  }
  
  if (nrow(datacheck[-1,]) != 0){# if dataframe has any rows
    return(kable(datacheck[-1,],format = "pipe", row.names=F, padding=50
                 , align = "lll", caption=paste0(header, " :")))# output table with header as caption
  }
  }


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