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
#' get the maximum value from a vector skipping NA values. If all values are NA returns NA.
#' 
#' @param x vector
#' @returns numeric
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' getMax(c(1,1))
#' getMax(c(NA,NA))
getMax <- function(x){
  # if all=NA, returns NA
  if (!all(is.na(x))) max(x, na.rm = TRUE) else NA
} 

#' getMin
#' 
#' get the minimum value from a vector skipping NA values. If all values are NA returns NA.
#' 
#' @param x vector
#' @returns numeric
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' getMin(c(1,1))
#' getMin(c(NA,NA))
getMin <- function(x) {
  # if all=NA, returns NA
  if (!all(is.na(x))) min(x, na.rm = TRUE) else NA
} 

