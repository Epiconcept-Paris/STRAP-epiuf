#
# Project Name : SARI-VEBIS
# Script Name  : printIfCompTab
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : Edits to printIf
# Date created : 10/03/2022
# Author       : JHD from GDS
# Date reviewed: 
# Reviewed by  : CLS

# Description --------------------------------------------------------------
# Receives parameters in form of list(cond = text) and outputs simple table. 
# Option to reference second dataset to mark duplicate variables
# 
# 
# 


# Changes Log --------------------------------------------------------------


# START of SCRIPT  --------------------------------------------------------

#' Title
#'
#' @param data The dataset
#' @param data_old A previously check dataset 
#' @param parameters ...
#' @param header The header to display before list of ID 
#' @param threshold The maximum number of ID to be displayed 
#' @param varname The ID varname
#'
#' @importFrom kableExtra kable
#' @return data.frame
#' @export
#'
#' @examples 
#' \dontrun{   printDataCheck(data=df, data_old=NULL,  parameters = parameters, 
#'                   header="Essential Data Checks", threshold=NULL , varname="idCheck") }
#'                   
printDataCheck<- function(data, data_old=NULL,  parameters, header="", threshold=NULL , varname="id"){ 
  # call in second dataset (that used in the last datacheck)
  
  if  (!varname %in% names(data) ){ # if varname is not in dataset, default to first line.
    varname <- names(data)[1]
    catret(paste0("Warning:", varname, "is not in dataset, first column used"))
  }
  
  if (is.null(threshold)) {threshold <- 50 } # default threshold is 50
  
  datacheck <- data.frame("Check"="", "Number"="", "Id" = "")#create empty dataframe 

  for (i in 1:length(parameters)){# loop through all condition text pairs in the list
    
    cond <- parse(text=names(parameters[i])) # retrieve condition from list
    text <-as.character(parameters[i]) # retrieve text caption from list

  Records <- subset(data,eval(cond),varname) # isolate records matching cond
  NbCond <- nrow(Records) # get count
  
  if (NbCond != 0){ # if have any records meeting cond...
  
    if (NbCond > 0 & NbCond < threshold){ # if number is less than treshold,
      if(!is.null(data_old)){# and a second dataset is provided
      
      Records_old <- subset(data_old,eval(cond),varname) # get records list from old set
      
      inOld <- intersect(Records[,varname], Records_old[,varname]) # list those in new that are also in old (ie are repeats)
      inNew <- setdiff(Records[,varname], inOld) # list those only in the new
      
      listIDold <-  unlist(inOld)
      listIDold <-  paste(listIDold, collapse = ", ") # collapse repeat IDs
      
      listIDnew <-  unlist(inNew)
      listIDnew <-  paste(listIDnew, collapse = ", ") # collapse new IDs
      
      if(nchar(listIDnew)>0){
        listID <- paste0("[ ", listIDold, " ]  **", listIDnew,"**") # combine to list IDs
      }else{listID <- paste0("[ ", listIDold, " ]")} # combine to list IDs
      
      }
      } else {# if no second dataset provided, output ids 
        listID <- paste(Records$IdCheck, collapse= ", ")
      }

    }else if(NbCond >= threshold){# if number of ids more than threshold, print message
      
      listID <- paste0(threshold, " or more")
    }
    
    RowToPrint <- c(text, NbCond,listID) # assmble row for each check
    datacheck <- rbind(datacheck, RowToPrint) # apprend to data frame
  }
  }
  if (nrow(datacheck[-1,]) != 0){# if dataframe has any rows
    return(kable(datacheck[-1,],format = "pipe", row.names=F, padding=50
                 , align = "lll", caption=paste0(header, " :")))# output table with header as caption
           }
  }



# Example
# parameters <- list( 
#   "is.na(Outcome)" = "**Missing outcome**" 
#   ,"Residence==2" = "**Lives in a residential home**")
# 
# 
# printDataCheck(data=df, data_old=NULL,  parameters = parameters, header="Essential Data Checks", threshold=NULL , varname="idCheck")
# 

# END of SCRIPT  --------------------------------------------------------
