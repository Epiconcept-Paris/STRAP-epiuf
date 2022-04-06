#
# Project Name : epiuf
# Script Name  : week_month_year
# GitHub repo  : epiuf
# Summary      : wrapper functions for isoweek and and isoyear
# Date created : 06/04/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# calYear - retrieves calender year
# isoYear - retrieves isoyear 
# abvMonth - retrieves 3 letter abreviated month
# isoWeek - retrieves iso week
# isoYearWeek - retrieves year+week in 2022w4 format
# Takes date variables, option to include validDate if not date.

# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' calYear
#'
#' @param data 
#' @param varname 
#'
#' @return variable
#' @export
#'
#' @examples
calYear <- function(data, varname){
  # enable non-character input of varname  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # if variable is not date, set to date.  
  # if (class(data[,varname])!="Date"){
  #   data[,varname] <- validDate(data[,varname])
  # }
  
  # Use base code to extract year from date  
  data[,varname] <- format(data[,varname], format="%Y")
  data[, varname]
}

#' isoYear
#'
#' @param data 
#' @param varname 
#'
#' @return variable
#' @export
#'
#' @examples
isoYear <- function (data, varname){
  # enable non-character input of varname  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # if variable is not date, set to date.  
  # if (class(data[,varname])!="Date"){
  #   data[,varname] <- validDate(data[,varname])
  # }
  
  # Use base code to extract week from date  
  data[,varname] <- strftime(data[,varname], format="%G")
  data[, varname]
}




#' abvMonth
#'
#' @param data 
#' @param varname 
#'
#' @return variable
#' @export
#'
#' @examples
abvMonth <- function(data, varname){
  # enable non-character input of varname  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # if variable is not date, set to date.  
  # if (class(data[,varname])!="Date"){
  #   data[,varname] <- validDate(data[,varname])
  # }
  
  # Use base code to extract month from date  
  data[,varname] <- format(data[,varname], format="%b")
  data[, varname]
}




#' isoWeek
#'
#' @param data 
#' @param varname 
#'
#' @return variable
#' @export
#'
#' @examples
isoWeek <- function(data, varname){
  # enable non-character input of varname  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # if variable is not date, set to date.  
  # if (class(data[,varname])!="Date"){
  #   data[,varname] <- validDate(data[,varname])
  # }
  
  # Use base code to extract week from date  
  data[,varname] <- strftime(data[,varname], format="%V")
  data[, varname]
}



#' isoYearWeek
#'
#' @param data 
#' @param varname 
#'
#' @return variable
#' @export
#'
#' @examples
isoYearWeek <- function(data, varname){
  # enable non-character input of varname  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  # if variable is not date, set to date.  
  # if (class(data[,varname])!="Date"){
  #   data[,varname] <- validDate(data[,varname])
  # }
  
  # Use base code to extract YEARwWEEK from date  
  # Use base code to extract YEARwWEEK from date  
  data[,varname] <- paste0(isoYear(data,varname), "w", isoWeek(data,varname))
  data[, varname]
}

# END of SCRIPT  --------------------------------------------------------