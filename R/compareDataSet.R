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


#' compareDataSet
#' 
#' Compare two dataset for difference in terms of structure
#'
#' @param modeldata A model dataset (one record at least)
#' @param data A dataset to test against the model
#'
#' @return a list of difference 
#' @export
#'
#' 
compareDataSet <-  function(modeldata, data) {
  
  cmod <- ncol(modeldata)
  ctest <- ncol(data)
  cat("Number of variables. Modele : ",cmod,"/ Tested :",ctest,"\n")
  
  colmod <-  colnames(modeldata)
  coltest <-  colnames(data)
  
  
  difnew <- as.data.frame(setdiff(coltest, colmod))
  bold(nrow(difnew)," New columns in tested :")
  catret(unlist(difnew))
  catret()
  difmiss <- as.data.frame(setdiff(colmod,coltest))
  
  bold(nrow(difmiss)," Missing columns in tested :")
  catret(unlist(difmiss))
  catret()
  
  tmod <- sapply(modeldata, typeof )
  ttest <- sapply(data, typeof)
  tmod <- cbind(colmod,tmod)
  ttest <- cbind(coltest,ttest)
  tcomp <- merge(tmod,ttest, by.x="colmod", by.y="coltest",all=TRUE)
  tcomp <- tcomp[is.na(tcomp$tmod)|is.na(tcomp$ttest)|(tcomp$tmod != tcomp$ttest),]
  colnames(tcomp) <- c("Variable","Modele","Tested")
  typedif <- nrow(tcomp)
  bold(typedif , " Variable with different type\n")
  tcomp
}


#' UpdateDataset
#' Transform a dataset by  adding/removing column according to a model dataset 
#' 
#' @param data A dataset to update 
#' @param modele A dataset used as model for the final structure 
#'
#' @return the updated dataset with modele structure 
#' @export
#'

updateDataset <- function(data, modele) {
  data.names <- names(data)
  modele.names <- names(modele)
  
  # columns in data but not in modele
  modele.drop <- setdiff(data.names, modele.names)
  
  # columns in modele but not in data
  data.add <- setdiff(modele.names, data.names)
  
  # remove unused column from data 
  data <- data[,! colnames(data) %in% modele.drop] 
  
  # add blank columns to data
  if(length(data.add) > 0) {
    for(i in 1:length(data.add)) {
      data[data.add[i]] <- NA
    }
  }
  return(data)
}

#' appendDataset
#'
#' @param maindata A dataset  for pooled data
#' @param datatoadd  A dataset to Add to pooled
#'
#' @return  The pooled dataset 
#' @export
#'
#' 
datatoaddset <- function(maindata, datatoadd) {
   maindata[setdiff(names(maindata),names(datatoadd))] <-  NA
   datatoadd[setdiff(names(datatoadd),names(maindata))] <-  NA
   result <- rbind(maindata,datatoadd)
}


# END of SCRIPT  --------------------------------------------------------