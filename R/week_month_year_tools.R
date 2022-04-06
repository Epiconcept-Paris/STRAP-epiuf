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

calYear <- function(date){
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
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract year from date  
  date <- format(date, format="%Y")
  date
}


isoYear <- function (date){
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
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- strftime(date, format="%G")
  date
}

abvMonth <- function(date){
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
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract month from date  
  date <- format(date, format="%b")
  date
}


isoWeek <- function(date){
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
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- strftime(date, format="%V")
  date
}


isoYearWeek <- function(date){
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
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract YEARwWEEK from date  
  date <- paste0(isoYear(date),"w", isoWeek(date))
  date
}

# END of SCRIPT  --------------------------------------------------------