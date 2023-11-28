# Project Name : epiuf
# Script Name  : RefSite.R
# GitHub repo  : STRAP-epiuf
# Summary      : 
# Date created : 2023-11-27
# Author       : Lore Merdrignac
# Date reviewed: 
# Reviewed by  : 

# Description -------------------------------------------------------------
# 

# Changes Log -------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' Get the ID code corresponding to the country or study site label
#' 
#' @param Label Character string to search among the "Label" column from epiuf::RefSite reference table
#' @param LocationName Character string to search among the "LocationName" column from epiuf::RefSite reference table
#' 
#' @returns Character string or a data frame if multiple answers
#' 
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' \dontrun{
#' getRefSiteID()
#' }
#' getRefSiteID(Label = "Austria")
#' getRefSiteID(LocationName = "Bosnia and Herzegovina")
#' getRefSiteID(Label = "The Czech Republic", LocationName = "Czechia")
#' \dontrun{
#' getRefSiteID(Label = "Czechia")
#' }
#' 
getRefSiteID <- function(Label, LocationName) {
  
  # Initialising the refsite dataframe
  refsite <- epiuf::RefSite
  
  if(missing(Label) & missing(LocationName)) {
    stop("Please provide getRefSiteID() either with Label or LocationName")
  }
  
  # Search based on Label
  if(!missing(Label) & missing(LocationName)) {
    results <- unique(refsite[refsite$Label %in% Label, ])
  }
  
  # Search based on LocationName
  if(missing(Label) & !missing(LocationName)) {
    results <- unique(refsite[refsite$LocationName %in% LocationName, ])
  }
  
  # Search based on both
  if(!missing(Label) & !missing(LocationName)) {
    results <- unique(refsite[(refsite$Label %in% Label) & (refsite$LocationName %in% LocationName), ])
  }
  
  if(nrow(results) == 1) {
    return(results$ID)
  }
  
  if(nrow(results) > 1) {
    warning("Several rows in RefSite match your search.")
    return(results)
  }
  
  if(nrow(results) == 0) {
    warning("Sorry but no rows in RefSite match your search.")
    return(NA)
  }
  
}


#' Get the label corresponding to the ID code of the country or study site label
#' 
#' @param ID Character string to search among the "ID" column from epiuf::RefSite reference table
#' 
#' @returns Character string or a data frame if multiple answers
#' 
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' \dontrun{
#' getRefSiteLabel()
#' }
#' getRefSiteLabel(ID = "SI")
#' getRefSiteLabel(ID = "UK")
#' \dontrun{
#' getRefSiteLabel(ID = "es")
#' }
#' 
getRefSiteLabel <- function(ID) {
  
  # Initialising the refsite dataframe
  refsite <- epiuf::RefSite
  
  if(missing(ID)) {
    stop("Please provide getRefSiteLabel() with ID to search for")
  }
  
  # Search based on ID
  if(!missing(ID)) {
    results <- unique(refsite[refsite$ID %in% ID, ])
  }
  
  if(nrow(results) == 1) {
    return(results$Label)
  }
  
  if(nrow(results) > 1) {
    warning("Several rows in RefSite match your search.")
    return(results)
  }
  
  if(nrow(results) == 0) {
    warning("Sorry but no rows in RefSite match your search.")
    return(NA)
  }
  
}




# END of SCRIPT  ----------------------------------------------------------