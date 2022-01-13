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


compareDataSet <-  function(dfmod, dftest) {
  
  cmod <- ncol(dfmod)
  ctest <- ncol(dftest)
  cat("Number of variables. Modele : ",cmod,"/ Tested :",ctest,"\n")
  
  colmod <-  colnames(dfmod)
  coltest <-  colnames(dftest)
  
  
  difnew <- as.data.frame(setdiff(coltest, colmod))
  cat("New columns in tested :",unlist(difnew),"\n")
  
  difmiss <- as.data.frame(setdiff(colmod,coltest))
  
  cat("Missing columns in tested :",unlist(difmiss),"\n")
  
  
  tmod <- sapply(dfmod, typeof )
  ttest <- sapply(dftest, typeof)
  tmod <- cbind(colmod,tmod)
  ttest <- cbind(coltest,ttest)
  tcomp <- merge(tmod,ttest, by.x="colmod", by.y="coltest",all=TRUE)
  tcomp <- tcomp[is.na(tcomp$tmod)|is.na(tcomp$ttest)|(tcomp$tmod != tcomp$ttest),]
  colnames(tcomp) <- c("Variable","Modele","Tested")
  typedif <- nrow(tcomp)
  cat(typedif , " Variable with different type\n\n")
  tcomp
}


# END of SCRIPT  --------------------------------------------------------