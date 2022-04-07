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
# distanceIsoWeeks - outputs number of whole weeks from an origin (specified automatially)
# Takes date variables, option to include validDate() if not date.

# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
calYear <- function(date){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract year from date  
  date <- as.numeric(format(date, format="%Y"))
  date
}


#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
isoYear <- function (date){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%G"))
  date
}

#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
abvMonth <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract month from date  
  date <- format(date, format="%b")
  date
}


#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
isoWeek <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%V"))
  date
}


#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
isoYearWeek <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract YEARwWEEK from date  
  date <- paste0(isoYear(date),"w", isoWeek(date))
  date
}


#' Title
#'
#' @param date 
#' @param origin 
#'
#' @return
#' @export
#'
#' @examples
distanceIsoWeeks <- function (date = date, origin = "2019-09-30"){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Find date of start of week (a monday by default)
  prevMonOrig <- as.Date(cut(as.Date(origin), "weeks"))
  prevMonDate <-as.Date(cut(as.Date(date), "weeks"))
  
  # calculate difference in weeks between origin and input date
  numberWeeks <- (prevMonDate-prevMonOrig)/7
  numberWeeks
}

# END of SCRIPT  --------------------------------------------------------


# END of SCRIPT  --------------------------------------------------------