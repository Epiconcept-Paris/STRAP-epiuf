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


# test a word against a list and return the most similar or NULL if not
#' Title
#'
#' @param ToTest a string which will be compraed to anotehr string or a list of strings
#' @param CorrectList a string or a list of string from which ToTest will be searched
#' @param ErrPerc An acceptable errPerc when comparing string , default to 0.10% 
#'
#' @return The string guessed from ToTest using CorrectList
#' @importFrom utils adist install.packages
#' @export
#'
#' @examples
#' 
#' verifySpelling("Janury",c("Juanuary", "Janvier" ))
#' 
verifySpelling <- function(ToTest,CorrectList,ErrPerc=0.25 ) {
    MyWord <-  function(x,value) {
      ToGrep <- paste0("\\<",x,"\\>")
      grep(ToGrep,value,ignore.case=TRUE)
    }
      
      
    WordIndex <-
    agrep(ToTest,
          CorrectList,
          max.distance = ErrPerc,
          ignore.case = TRUE)
  if (length(WordIndex) > 1) {
    # more than one is matching
    # if (length(WordIndex) < 3) {
    #   # if two are matching, we take the first
    #   WordIndex <- WordIndex[1]
    # }
    # else {
    #   # many matching we use adist partial = False
    Dist <- adist(ToTest, CorrectList, ignore.case = TRUE)
    if (min(Dist) >= nchar(ToTest)) {
      # Distance is greater than ToTest we cannot conclude
      WordIndex <- 0
    } else {
      WordIndex <-
        which.min(Dist)     # we use the value with the smaller distance
    }
    # }
  }
  if (length(WordIndex)==0) {
    WordIndex <- which(lapply(names(CorrectList),MyWord,value=ToTest) == 1)
    if (length(WordIndex) > 1) WordIndex <- WordIndex[1]
  }  
  if (length(WordIndex)==0) {WordIndex <- 0}
  # Wordindex is integer(0) empty if not found
  if (WordIndex==0 ) {
    return(NA)
  } else {
    if (is.null(names(CorrectList)))  {
      #No names then we use the founded values
      return(CorrectList[WordIndex])
    } else {
      # we return the name
      return(names(CorrectList)[WordIndex])
    }
  }  
}

# END of SCRIPT  --------------------------------------------------------