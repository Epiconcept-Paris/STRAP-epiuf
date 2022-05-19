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
# abrvMonth - retrieves 3 letter abreviated month
# isoWeek - retrieves iso week
# isoYearWeek - retrieves year+week in 2022w4 format
# countIsoWeeks - outputs number of whole weeks from an origin (specified automatially)
# Takes date variables, option to include validDate() if not date.


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' calYear
#'
#' @param date A date value
#'
#' @return The calendar Year of the date 
#' @export
#'
#' @examples
#' calYear(Sys.Date())
calYear <- function(date){
  
  # if variable is not date, give warning.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract year from date  
  date <- as.numeric(format(date, format="%Y"))
  date
}


#' isoYear 
#'
#' @param date A date value
#'
#' @return The isoYear of the date value which may be different from the
#'         calendar Year 
#' @export
#'
#' @examples
#' isoYear(Sys.Date())
isoYear <- function (date){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%G"))
  date
}

#' abvMonth 
#'
#' @param date  A date Value
#'
#' @return The month of the date in abreviated format 
#' @export
#'
#' @examples
#' abvMonth(Sys.Date())
abrvMonth <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract month from date  
  date <- format(date, format="%b")
  date
}


#' isoWeek
#'
#' @param date A date Value
#'
#' @return The iso Week Number
#' @export
#'
#' @examples
#' isoWeek(Sys.Date())
isoWeek <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%V"))
  date
}


#' isoYearWeek
#'
#' @param date A date Value
#'
#' @return The iso year week of the Date  in xx w yyyy format  (43w2020) 
 #' @export
#'
#' @examples
#' isoYearWeek(Sys.Date())
isoYearWeek <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract YEARwWEEK from date  
  date <- paste0(isoYear(date),"w", isoWeek(date))
  date <- ifelse(date=="NAwNA", NA, date) # if output is NA, set it to be so
  date
}


#' countIsoWeeks
#'
#' @param date A date Value
#' @param origin A reference date value
#'
#' @return the nimber of isoweek between the two dates
#' @export
#'
#' @examples
#' countIsoWeeks(date = Sys.Date(),origin = "2000-01-01")
countIsoWeeks <- function (date = date, origin = "2019-09-30"){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  if( all(is.na(date))){ NA } else {  
  # Find date of start of week (a monday by default)
  prevMonOrig <- as.Date(cut(as.Date(origin), "weeks"))
  prevMonDate <-as.Date(cut(as.Date(date), "weeks"))
  
  # calculate difference in weeks between origin and input date
  numberWeeks <- (prevMonDate-prevMonOrig)/7
  numberWeeks
  }
}

# END of SCRIPT  --------------------------------------------------------


# END of SCRIPT  --------------------------------------------------------