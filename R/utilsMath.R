#
# Project Name : STRAP Project
# Script Name  : utilsMath.R
# GitHub repo  : STRAP-epiuf
# Summary      : Set of maths functions used widely
# Date created : 2023-05-24
# Author       : Cristina Lopez (epi-clz)
# Date reviewed: 2023-05-31
# Reviewed by  : LMC

# Description --------------------------------------------------------------
# 
# Set of maths functions used widely
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' getMax
#' 
#' Get the maximum value from a vector skipping NA values. If all values are NA, returns NA.
#' 
#' @param ... numeric or character vector arguments
#' 
#' @returns single numeric or character element
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [base::max()] and [epiuf::getMin()], [epiuf::getMean()], 
#' [epiuf::getMedian()] from `epiuf` package
#'
#' @examples
#' # Numerics
#' getMax(c(1, 9))
#' getMax(1, 9, NA) #c() is not mandatory
#' getMax(as.Date("2023-01-01"), 
#'        NA, 
#'        as.Date("2023-01-31"))
#' 
#' # Characters
#' getMax(LETTERS) 
#' ## Note: For characters, getMax() will return the last month 
#' ## according to alphabetical order (i.e., "September")
#' getMax(month.name) 
#' getMax(NA, month.name)
#' 
#' # If missing values
#' ## using basic max() function from base R
#' max(c(1:9, NA)) #NA value takes over the other values by default
#' max(c(1:9, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the max value despite NAs
#' max(c(NA, NA), na.rm = TRUE) #If NAs only, returns Inf and a warning
#' ## using getMax, we do not have these issues
#' getMax(c(NA, 1:9))
#' getMax(c(NA, NA))
#' 
getMax <- function(...){
  
  # if all = NA, returns NA
  if (!all(is.na(c(...)))) {
    result <- max(na.rm = TRUE, ...)
  } else {
    result <- NA
  }
  
  return(result)
} 



#' getMin
#' 
#' Get the minimum value from a vector skipping NA values. If all values are NA, returns NA.
#' 
#' @param ... numeric or character vector arguments
#' 
#' @returns single  numeric or character element
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [base::min()] and [epiuf::getMax()], [epiuf::getMean()], 
#' [epiuf::getMedian()] from `epiuf` package
#'
#' @examples
#' # Numerics
#' getMin(c(1, 9))
#' getMin(1, 9, NA) #c() is not mandatory
#' getMin(as.Date("2023-01-01"), 
#'        NA, 
#'        as.Date("2023-01-31"))
#' 
#' # Characters
#' getMin(LETTERS)
#' ## Note: For characters, getMin() will return the first month 
#' ## according to alphabetical order (i.e., "April") 
#' getMin(month.name)
#' getMin(NA, month.name)
#' 
#' # If missing values
#' ## using basic min() function from base R
#' min(c(1:9, NA)) #NA value takes over the other values by default
#' min(c(1:9, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the min value despite NAs
#' min(c(NA, NA), na.rm = TRUE) #If NAs only, returns Inf and a warning
#' ## using getMin, we do not have these issues
#' getMin(c(NA, 1:9))
#' getMin(c(NA, NA))
#' 
getMin <- function(...) {
  
  # if all=NA, returns NA
  if (!all(is.na(c(...)))) {
    result <- min(na.rm = TRUE, ...)
  } else {
    result <- NA
  }
  
  return(result)
} 



#' getMean
#' 
#' Get the arithmetic mean value from a vector skipping NA values. If all values are NA, returns NA.
#' 
#' @param ... numeric, logical, date vector
#' 
#' @returns single numeric, logical, date mean value
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [base::mean()] and [epiuf::getMin()], [epiuf::getMax()], 
#' [epiuf::getMedian()] from `epiuf` package
#'
#' @examples
#' # Numerics
#' getMean(c(1, 9))
#' getMean(c(1, 9, NA))
#' getMean(c(as.Date("2023-01-01"), 
#'          NA, 
#'          as.Date("2023-01-31")))
#' 
#' 
#' # If missing values
#' ## using basic mean() function from base R
#' mean(c(1:9, NA)) #NA value takes over the other values by default
#' mean(c(1:9, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the min value despite NAs
#' mean(c(NA, NA), na.rm = TRUE) #If NAs only, returns NaN
#' ## using getMean, we do not have these issues
#' getMean(c(NA, 1:9))
#' getMean(c(NA, NA))
#' 
getMean <- function(...) {
  
  # If all = NA, returns NA
  if (!all(is.na(c(...)))) {
    result <- mean(c(...), na.rm = TRUE)
  } else {
    result <- NA
  }
  
  return(result)
  
} 



#' getMedian
#' 
#' Get the median value from a vector skipping NA values. If all values are NA, returns NA.
#' 
#' @param ...  numeric, logical, date vector
#' 
#' @returns single numeric, logical, date median value
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [stats::median()] and [epiuf::getMean()], [epiuf::getMin()], [epiuf::getMax()] from `epiuf` package
#'
#' @examples
#' # Numerics
#' getMedian(c(1, 9))
#' getMedian(c(1, 9, NA))
#' getMedian(c(as.Date("2023-01-01"), 
#'          NA, 
#'          as.Date("2023-01-31")))
#' 
#' 
#' # If missing values
#' ## using basic mean() function from base R
#' median(c(1:9, NA)) #NA value takes over the other values by default
#' median(c(1:9, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the min value despite NAs
#' median(c(NA, NA), na.rm = TRUE)
#' ## using getMedian, we do not have this issue
#' getMedian(c(NA, 1:9))
#' getMedian(c(NA, NA))
#' 
getMedian <- function(...) {
  
  # If all = NA, returns NA
  if (!all(is.na(c(...)))) {
    result <- median(c(...), na.rm = TRUE)
  } else {
    result <- NA
  }
  
  return(result)
  
} 