#
# Project Name : STRAP 
# Script Name  : viewIf.R
# GitHub repo  : epiuf
# Summary      : extended view function
# Date created : 2022/02/17
# Author       : GDE
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
#  This function open a view in R to show an extract of a dataset 
#  with some selected var and some lines from head or from end 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' @title List selected column with an optional condition
#' 
#' @description List head or tail of a data.frame allowing to select column to be viewed and an optional condition to select 
#' rows to be displayed.
#' 
#' `viewIf` allow to quickly display some column of a dataframe satisfiyng a condition. It can be used during
#'  data exploration or after a recoding to verify the result
#'  When a condition is given, viewIf return all rows for which the condition is TRUE plus all rows with NA 
#' 
#'
#' @param data The dataframe to explore
#' @param ... variable name(s) to include in view, as character
#' @param cond A logical condition to select records which will be evaluated into data, given as character 
#' string. The condition should be passed between quote : cond="mycondition". 
#' @param nline Number of rows to show (default to 10) If nline is negative then `viewIf` display nline rows
#'  from the end (similar to tail function) 
#'
#' @return the resulting selection of n rows for the given list of column
#' @export
#'
#' @examples
#' data <- data.frame(id = 1:10,
#'                    case = c(rep(1,3), rep(0,7)),
#'                    vacc = sample(c(0,1), replace = TRUE, size = 10),
#'                    ill = sample(c(0,1), replace = TRUE, size = 10))
#' data[8,2]<-NA                    
#' viewIf(data ,case,vacc)
#' viewIf(data, ,case,vacc, nline=3)
#' viewIf(data,case,ill, nline= -3)
#' viewIf(data,case,ill,cond="vacc==1" )
#'  

viewIf <- function(data,...,cond=NULL,nline=10) {
  r <- as.list(match.call())
  n <- names(r)
  if(is.null(cond)) cond<-TRUE 
  else cond <- substitute(cond)
  if (!typeof(cond)=="language") {cond <- parse(text=cond)}
  
  imatch <-pmatch(c("nline","data","cond"),n, nomatch = 0)
  if (length(imatch) > 0) {
    r[imatch] <- NULL
  }
  l <- length(r)
  c <- vector()
  j <- 0
  for (i in 2:l) {
    j <- j+1
    colname <- as.character(r[i]) 
    if (colname %in% names(data)) {
      c <- rbind(c, colname)
    } else {
      red("column ",colname," is not in dataset ")
    }
  }
  res <- as.data.frame(data[with(data,eval(cond)),c])
  # res <- na.omit(res)
  if (nline > 0) {
    if(nline > nrow(res)) nline<-nrow(res)
    res <- as.data.frame(res[1:nline,])
  } else {
    l <- nrow(data)
    nline <- l + nline -1
    if(abs(nline) > nrow(res)) nline<- -nrow(res)
    res <- as.data.frame(res[nline:l,])
  }
  if(ncol(res)==1) colnames(res)<-colname
  return(res)
}




# END of SCRIPT  --------------------------------------------------------