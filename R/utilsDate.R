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
# isoYearWeek - retrieves year+week in 2022w04 format
# countIsoWeeks - outputs number of whole weeks from an origin (specified automatially)
# Takes date variables, option to include validDate() if not date.


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' calYear
#' 
#' Extract the year of a date object and return it as numeric
#'
#' @param date A date value
#'
#' @returns The calendar Year of the date as numeric
#' 
#' @export
#' 
#' @seealso [epiuf::isoYear()] and [base::format()]
#'
#' @examples
#' calYear(Sys.Date())
#' 
calYear <- function(date){
  
  # if variable is not date, give warning.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract year from date  
  date <- as.numeric(format(date, format="%Y"))
  
  return(date)
}


#' isoYear 
#' 
#' Extract the ISO year of a date object and return it as numeric
#'
#' @param date A date value
#'
#' @returns The ISO year of the date value which may be different from the
#'         calendar Year 
#'         
#' @export
#' 
#' @seealso [epiuf::calYear()], [epiuf::isoYearWeek()], [epiuf::isoWeek()] 
#' and [base::format()]
#'
#' @examples
#' isoYear(Sys.Date())
#' isoYear(as.Date("2019-12-31"))
#' 
isoYear <- function (date){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%G"))
  
  return(date)
}


#' abrvMonth 
#' 
#' Returns the month in abbreviated format of a date object
#'
#' @param date  A date Value
#'
#' @returns The month of the date in abbreviated format 
#' 
#' @export
#' 
#' @seealso [epiuf::Month()] and [base::format()]
#' 
#' @examples
#' abrvMonth(Sys.Date())
#' 
abrvMonth <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract month from date  
  date <- format(date, format="%b")
  
  return(date)
}


#' Month 
#'
#' Returns the month in numeric format of a date object
#' 
#' @param date  A date Value
#'
#' @returns The month of the date in numeric
#' 
#' @export
#' 
#' @seealso [epiuf::abrvMonth()] and [base::format()]
#'
#' @examples
#' Month(Sys.Date())
#' 
Month <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract month from date  
  date <- as.numeric(format(date, format="%m"))
  
  return(date)
}


#' isoWeek
#' 
#' Returns the ISO week number in numeric format of a date object
#'
#' @param date A date Value
#'
#' @returns The iso Week Number
#' 
#' @export
#' 
#' @seealso [epiuf::isoYear()], [epiuf::isoYearWeek()]  and [base::format()]
#'
#' @examples
#' isoWeek(Sys.Date())
#' isoWeek(as.Date("2020-12-31"))
#' 
isoWeek <- function(date){
  
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Use base code to extract week from date  
  date <- as.numeric(strftime(date, format="%V"))
  
  return(date)
}


#' isoYearWeek
#' 
#' Returns the ISO year and week in character string format (e.g., 2020w03) of a date object
#'
#' @param date A date Value
#' @param weekformat A string to represent the week separator. Default is "w". 
#' May be one or more characters (eg : "-W") 
#'
#' @return The iso year week of the Date  in YYYYwXX format  (2020w03) 
#' 
#' @export
#' 
#' @seealso [epiuf::isoYear()], [epiuf::isoWeek()], [epiuf::lpad()] and [base::format()]
#'
#' @examples
#' isoYearWeek(Sys.Date())
#' isoYearWeek(as.Date("2020-12-31"))
#' 
isoYearWeek <- function(date,weekformat=NULL){  # Use base code to extract YEARwWEEK from date 
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  # Setting the week separator
  if (is.null(weekformat)) weekformat <- "w"
  
  # paste Week and year together
  date <- ifelse(is.na(isoYear(date)), NA_character_,
                 paste0(isoYear(date), weekformat, lpad(isoWeek(date), 2, char = "0")))
  
  return(date)
}


#' countIsoWeeks
#'
#' Return the number of weeks between the two dates
#' 
#' @param date A date Value
#' @param origin A reference date value in character string format
#'
#' @return the number of weeks between the two dates
#' 
#' @export
#'
#' @seealso [base::difftime()]
#' 
#' @author Jenny Howard \email{j.howard@epiconcept.fr}
#' 
#' @examples
#' countIsoWeeks(date = Sys.Date(), origin = "2000-01-01")
#' 
countIsoWeeks <- function (date, origin = "2020-10-05"){
  
  # if variable is not date, set to date.  
  # if (class(date)!="Date"){
  #   date <- validDate(date)
  # }
  
  if( all(is.na(date))){ 
    
    return(NA) 
    
  } else {  
    
    # Find date of start of week (a monday by default)
    prevMonOrig <- as.Date(cut(as.Date(origin), "weeks"))
    prevMonDate <-as.Date(cut(as.Date(date), "weeks"))
    
    # calculate difference in weeks between origin and input date
    # numberWeeks <- (prevMonDate-prevMonOrig)/7
    numberWeeks <- difftime(prevMonDate, prevMonOrig, units = "weeks")
    
    return(numberWeeks)
    
  }
}



#' lastDateMonth
#' 
#' Takes monthly dates in character string such as "dec2022" and returns the date of 
#' the last day of the month in date format 
#'
#' @param month character string, abbreviated month+year (eg. dec2022)
#' @param lc_time character string, input of the `Sys.setlocale("LC_TIME", lc_time)`,
#' required if the system local language for the time is not the same as the `month` argument
#' (e.g., if Sys.getlocale("LC_TIME") is "French_France.utf8", but month = "dec2022").
#' Default value being "C" for English format.
#' 
#' @return The last date in that month in date format
#' 
#' @export
#' 
#' @author Jenny Howard \email{j.howard@epiconcept.fr}
#'
#' @examples
#' lastDateMonth("dec2022")
#' 
lastDateMonth <- function(month, lc_time = "C"){
  
  # separate year and month
  
  ## convert if needed the system local language for dates
  lc_time0 <- Sys.getlocale("LC_TIME")
  if(lc_time0 != lc_time) {
    Sys.setlocale("LC_TIME", lc_time)
  }
  
  # get date for 1st of month
  d <- as.Date(paste0("01",month), format="%d%b%Y")
  
  # Get year
  y <- calYear(d)
  
  # find date for 1st of next month
  m2 <- Month(as.Date(d+31)) # retrieve next month
  if(m2==1){y = y+1} # if next month is in next year, edit year
  d2 <- as.Date(paste0("01-", m2,"-", y), format = "%d-%m-%Y") # get first date for the year
  
  lastday <- d2-1 # get last day of the given month
  
  ## convert back to the initial language
  if(lc_time0 != lc_time) {
    Sys.setlocale("LC_TIME", lc_time0)
  }
  
  return(lastday)
  
}


#' isDate
#' 
#' Uses the base function class to asses if an object is a date or not
#' 
#' @param date date object or variable that wish to test
#' @returns logical
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' isDate(epiuf::DummyData$EnrolmentDate)
isDate <- function(date){
  if(!is.null(date)){
  output <- class(date) %in% c("Date", "POSIXt")
  }else{stop("Object doesn't exist")}
  
  return(output)
}

# END of SCRIPT  --------------------------------------------------------