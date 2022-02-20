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
#' @param date1 The first date 
#' @param date2 The second date
#' @param ... one or more logical condition to validate. If one of the condition is not TRUE, 
#'            NA is returned instead of the calculated delay 
#'
#' @return a vector of the same length than the passed dataset
#' @export
#'
#' @examples
#' \dontrun{
#'    getDelayIf(data ,date1,date2, date2 > date1 + 14)
#' }
#' 
getDelayIf <- function(data, date1, date2, ...) {
  
  # first we catch the ... parameters
  listcond <- as.list(match.call())
  # we remove the first one which is the function name
  listcond[1] <- NULL
  # and we remove all named parameters from the list
  namedarg <- pmatch(c("data","date1","date2"),names(listcond), nomatch = 0)
  if (length(namedarg) > 0 ) {listcond[namedarg] <- NULL }
  
  # We verify that parameters are language and if it is "test" then we parse as language
  for (i in 1:length(listcond)) {
    if (!typeof(listcond[i])=="language") {listcond[i] <- parse(text=listcond[i])}
  }
  
  # we get the named parameters as "parsed language"
  s_op <- deparse(substitute(date1))
  # if date1 is a variable which contain char, we use content of date1
  tryCatch(
    if (is.character(date1)) {
      s_op <- date1
    }
    , error = function(c) { }
  )
  date1 <- s_op
  
  # we get the named parameters as "parsed language"
  s_op <- deparse(substitute(date2))
  # if date1 is a variable which contain char, we use content of date1
  tryCatch(
    if (is.character(date2)) {
      s_op <- date2
    }
    , error = function(c) { }
  )
  date2 <- s_op
  
  
  # we add a delay variable to the dataset (should we ?) 
  data$temp__delay <- as.numeric(data[, date2] - data[, date1])
  
  # we loop over condition 
  for (i in 1:length(listcond)) {
    onecond = listcond[[1]]
    # we get index of row which meet the condition 
    meet <- which(with(data,eval(onecond)))
    # delay is NA if the condition is not meet
    data[ ! meet,"temp__delay"] <- NA
  }
  result <- data$temp__delay
  data$temp__delay <- NULL
  
  return(result)
}

# END of SCRIPT  --------------------------------------------------------

