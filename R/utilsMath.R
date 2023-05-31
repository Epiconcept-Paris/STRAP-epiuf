#
# Project Name : STRAP Project
# Script Name  : utilsMath.R
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

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
#' @param x numeric or character vector
#' 
#' @returns single numeric or character element
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [base::max()] and [epiuf::getMin()]
#'
#' @examples
#' # Numerics
#' getMax(c(1, 9))
#' getMax(c(1, 9, NA))
#' getMax(c(as.Date("2023-01-01"), 
#'          NA, 
#'          as.Date("2023-01-31")))
#' 
#' # Characters
#' getMax(LETTERS) 
#' getMax(month.name)
#' getMax(c(NA, month.name))
#' 
#' # If missing values
#' ## using basic max() function from base R
#' max(c(1:5, NA)) #NA value takes over the other values by default
#' max(c(1:5, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the max value despite NAs
#' max(c(NA,NA), na.rm = TRUE) #If NAs only, returns Inf and a warning
#' ## using getMax, we do not have these issues
#' getMax(c(1:5, NA))
#' getMax(c(NA, NA))
#' 
getMax <- function(x){
  # if all = NA, returns NA
  if (!all(is.na(x))) max(x, na.rm = TRUE) else NA
} 


#' getMin
#' 
#' Get the minimum value from a vector skipping NA values. If all values are NA, returns NA.
#' 
#' @param x numeric or character vector
#' 
#' @returns single  numeric or character element
#' 
#' @export
#' 
#' @author STRAP team \email{strap@epiconcept.fr}
#' 
#' @seealso [base::min()] and [epiuf::getMax()]
#'
#' @examples
#' # Numerics
#' getMin(c(1, 9))
#' getMin(c(1, 9, NA))
#' getMin(c(as.Date("2023-01-01"), 
#'          NA, 
#'          as.Date("2023-01-31")))
#' 
#' # Characters
#' getMin(LETTERS) 
#' getMin(month.name)
#' getMin(c(NA, month.name))
#' 
#' # If missing values
#' ## using basic min() function from base R
#' min(c(1:5, NA)) #NA value takes over the other values by default
#' min(c(1:5, NA), na.rm = TRUE) #Need to add na.rm = TRUE to get the min value despite NAs
#' min(c(NA,NA), na.rm = TRUE) #If NAs only, returns Inf and a warning
#' ## using getMin, we do not have these issues
#' getMin(c(1:5, NA))
#' getMin(c(NA, NA))
#' 
getMin <- function(x) {
  # if all=NA, returns NA
  if (!all(is.na(x))) min(x, na.rm = TRUE) else NA
} 

