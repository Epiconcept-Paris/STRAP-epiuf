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
#' This function returns the delay between two dates with a condition.
#' If the condition is validated then the delay in days from date one to date two is returned, 
#' else NA is returned.  
#'
#' @param data The dataset containing the values (data.frame).
#' @param FirstDateName The first date (as.Date).
#' @param SecondDateName The second date (as.Date).
#' @param ... One or more logical condition to validate. If one of the condition is not TRUE, 
#'            NA is returned instead of the calculated delay. 
#'
#' @return A vector with the same length of the input dataset.
#' @export
#'
#' @examples
#' 
#' # Create a random data frame with two date variables
#' df <- data.frame(date1 = c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")
#' ,date2 = c("2023-02-01", "2023-02-02", "2023-04-03", "2023-03-04")
#' ,id = c("a", "b", "c", "d"))
#' 
#' # Convert them to the correct format 
#' df$date1 <- as.Date(df$date1)
#' df$date2 <- as.Date(df$date2)
#' 
#' # Use the function 
#' df$test <- ifelse(getDelayIf(df, date1, date2)>40, 1, 0) 
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
  dataType <- unique(class(data))
  if(length(dataType) > 1 | (length(dataType) == 1 & dataType != "data.frame")){
    stop(data, " is not a data frame")
  }

  # # we add a delay variable to the dataset (should we ?) 
  # tryCatch(
  #   data$temp_dellay <- as.numeric(data[, SecondDateName] - data[, FirstDateName])
  #   , error = function(c) { stop(FirstDateName," or ",SecondDateName,"  is not a valid date for getDelayIf" )}
  # )
  
  
  # check that variables format is Date
  if (!epiuf::isDate(data[, FirstDateName])) {
    stop(FirstDateName," is not Date format")
  }
  if (!epiuf::isDate(data[, SecondDateName])) {
    stop(SecondDateName," is not Date format")
  }
  
  
  # We loop over condition if one exist 
  if (length(listcond) > 0) {
    
    for (i in 1:length(listcond)) {
      
      onecond = listcond[[1]]
      
      # we get index of row which meet the condition 
      meet <- which(with(data,eval(onecond)))
      
      # Calculate delay for those meeting the condition
      data[meet,"temp_delay"] <- as.numeric(data[meet, SecondDateName] - data[meet, FirstDateName])
    }
  } else{
    
    # If there is no condition, we calculate the delay for all
    data[,"temp_delay"] <- as.numeric(data[, SecondDateName] - data[, FirstDateName])
    
  }
  
  result <- data$temp_delay
  
  data$temp_delay <- NULL
  
  return(result)
}

# END of SCRIPT  --------------------------------------------------------

