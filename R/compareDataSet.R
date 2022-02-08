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
#' @param dfmod A model dataset (one record at least)
#' @param dftest A dataset to test against the model
#'
#' @return a list of difference 
#' @export
#'
#' 

compareDataSet <-  function(dfmod, dftest) {
  
  cmod <- ncol(dfmod)
  ctest <- ncol(dftest)
  cat("Number of variables. Modele : ",cmod,"/ Tested :",ctest,"\n")
  
  colmod <-  colnames(dfmod)
  coltest <-  colnames(dftest)
  
  
  difnew <- as.data.frame(setdiff(coltest, colmod))
  bold(nrow(difnew)," New columns in tested :")
  catret(unlist(difnew))
  catret()
  difmiss <- as.data.frame(setdiff(colmod,coltest))
  
  bold(nrow(difmiss)," Missing columns in tested :")
  catret(unlist(difmiss))
  catret()
  
  tmod <- sapply(dfmod, typeof )
  ttest <- sapply(dftest, typeof)
  tmod <- cbind(colmod,tmod)
  ttest <- cbind(coltest,ttest)
  tcomp <- merge(tmod,ttest, by.x="colmod", by.y="coltest",all=TRUE)
  tcomp <- tcomp[is.na(tcomp$tmod)|is.na(tcomp$ttest)|(tcomp$tmod != tcomp$ttest),]
  colnames(tcomp) <- c("Variable","Modele","Tested")
  typedif <- nrow(tcomp)
  bold(typedif , " Variable with different type\n")
  tcomp
}


# END of SCRIPT  --------------------------------------------------------