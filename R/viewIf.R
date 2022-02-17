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

#' @title  List head of data.frame allowing to select column to view
#'
#' @param data The dataset to explore
#' @param ... variable name(s) to include in view
#' @param cond A logical condition to select records
#' @param nline Number of rows to show (default to n) If negative then number of row from the end
#'
#' @return invisibly, the resulting selection
#' @export
#'
#' @examples
#' \dontrun{
#' view(data ,age,case)
#' view(data, ,case, nline=5)
#' view(data, age,case, nline= -10)
#' }

viewIf <- function(data,...,cond=NULL,nline=10) {
  r <- as.list(match.call())
  n <- names(r)
  cond <- substitute(cond)
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
  if (nline > 0) {
    res <- data[with(data,eval(cond)),]
    res <- as.data.frame(res[1:nline,c])
  } else {
    l <- nrow(data)
    nline <- l + nline
    res <- data[with(data,eval(cond)),]
    res <- as.data.frame(res[nline:l,c])
  }
  View(res)
  invisible(res)
}




# END of SCRIPT  --------------------------------------------------------