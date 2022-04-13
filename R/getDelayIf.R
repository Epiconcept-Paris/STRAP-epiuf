#
# Project Name : STRAP
# Script Name  : getDelayIf
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of getDelayIf function
# Date created : 16/02/2022
# Author       : JHD GDE
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to retreive delay in dates between any two given dates, and 
# enables specifications of conditions that need to be met, otherwise NA given.
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------



#' getDelayIf
#' 
#' return the delay between two date with condition
#' if condition are validated then the delay in days from date one to date two is returned 
#' else NA is returned  
#'
#' @param data The dataset containing the values
#' @param FirstDateName The first date 
#' @param SecondDateName The second date
#' @param ... one or more logical condition to validate. If one of the condition is not TRUE, 
#'            NA is returned instead of the calculated delay 
#'
#' @return a vector of the same length than the passed dataset
#' @export
#'
#' @examples
#' \dontrun{
#'    getDelayIf(data ,FirstDateName,SecondDateName, SecondDateName > FirstDateName + 14)
#' }
#' 
getDelayIf <- function(data, FirstDateName, SecondDateName, ...) {
  
  # first we catch the ... parameters
  listcond <- as.list(match.call())
  # we remove the first one which is the function name
  listcond[1] <- NULL
  # and we remove all named parameters from the list
  namedarg <- pmatch(c("data","FirstDateName","SecondDateName"),names(listcond), nomatch = 0)
  if (length(namedarg) > 0 ) {listcond[namedarg] <- NULL }
  
  # We verify that parameters are language and if it is "test" then we parse as language
  if (length(listcond)>0) {
    for (i in 1:length(listcond)) {
      if (!typeof(listcond[i])=="language") {listcond[i] <- parse(text=listcond[i])}
    }
  }  
  # we get the named parameters as "parsed language"
  s_op <- deparse(substitute(FirstDateName))
  # if FirstDateName is a variable which contain char, we use content of FirstDateName
  tryCatch(
    if (is.character(FirstDateName)) {
      s_op <- FirstDateName
    }
    , error = function(c) { }
  )
  FirstDateName <- s_op
  
  # we get the named parameters as "parsed language"
  s_op <- deparse(substitute(SecondDateName))
  # if FirstDateName is a variable which contain char, we use content of FirstDateName
  tryCatch(
    if (is.character(SecondDateName)) {
      s_op <- SecondDateName
    }
    , error = function(c) { }
  )
  SecondDateName <- s_op
  
  # check that variables names are ok otherwise stop
  if (! FirstDateName %in% names(data)) {
     stop(FirstDateName," is not a valid column name")
  }
  if (! SecondDateName %in% names(data)) {
    stop(SecondDateName," is not a valid column name")
  }
  
  # Check if data is a data.frame
  if(!dplyr::is.tbl(data) | !is.data.frame(data)){
    stop(data, "is not a data frame")
  }

  # we add a delay variable to the dataset (should we ?) 
  tryCatch(
    data$temp__delay <- as.numeric(data[, SecondDateName] - data[, FirstDateName])
    , error = function(c) { stop(FirstDateName," or ",SecondDateName,"  is not a valid date for getDelayIf" )}
  )
  
  
  # we loop over condition if one exist
  if (length(listcond)>0) {
    for (i in 1:length(listcond)) {
      onecond = listcond[[1]]
      # we get index of row which meet the condition 
      meet <- which(with(data,eval(onecond)))
      # delay is NA if the condition is not meet
      data[ ! meet,"temp__delay"] <- NA
    }
  }  
  result <- data$temp__delay
  data$temp__delay <- NULL
  
  return(result)
}

# END of SCRIPT  --------------------------------------------------------

