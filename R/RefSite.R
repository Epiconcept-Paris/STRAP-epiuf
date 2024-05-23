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

#' Get the ID code corresponding to the site Label or LocationName
#' 
#' Get the ID code corresponding to the country/study site label from \code{epiuf::RefSite} reference table
#' 
#' @param Label Character string to search in the \code{"Label"} column from \code{epiuf::RefSite} reference table
#' @param LocationName Character string to search in the \code{"LocationName"} column from \code{epiuf::RefSite} reference table
#' 
#' @returns Character string or a data frame if multiple answers
#' 
#' @export
#' @author Lore Merdrignac \email{l.merdrignac@epiconcept.fr}
#' @seealso
#' \itemize{
#'  \item \code{\link{getRefSiteLabel}}
#'  \item \code{\link{getRefSiteLocName}}
#' }
#' For more details see the corresponding vignette: \code{vignette("RefSite")}
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


#' Get the Label corresponding to the site ID
#' 
#' Get the Label corresponding to the ID code of the country/study site from \code{epiuf::RefSite} reference table
#' 
#' @param ID Character string to search in the \code{"ID"} column from \code{epiuf::RefSite} reference table
#' 
#' @returns Character string or a data frame if multiple answers
#' 
#' @export
#' @author Lore Merdrignac \email{l.merdrignac@epiconcept.fr}
#' @seealso
#' \itemize{
#'  \item \code{\link{getRefSiteLocName}}
#'  \item \code{\link{getRefSiteID}}
#' }
#' For more details see the corresponding vignette: \code{vignette("RefSite")}
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


#' Get the LocationName corresponding to the site ID
#' 
#' Get the LocationName corresponding to the ID code of the country/study site from \code{epiuf::RefSite} reference table
#' 
#' @param ID Character string to search in the \code{"ID"} column from \code{epiuf::RefSite} reference table
#' 
#' @returns Character string or a data frame if multiple answers
#' 
#' @export
#' @author Lore Merdrignac \email{l.merdrignac@epiconcept.fr}
#' @seealso
#' \itemize{
#'  \item \code{\link{getRefSiteLabel}}
#'  \item \code{\link{getRefSiteID}}
#' }
#' For more details see the corresponding vignette: \code{vignette("RefSite")}
#'
#' @examples
#' \dontrun{
#' getRefSiteLocName()
#' }
#' getRefSiteLocName(ID = "SI")
#' getRefSiteLocName(ID = "UK")
#' \dontrun{
#' getRefSiteLocName(ID = "es")
#' }
#' 
getRefSiteLocName <- function(ID) {
  
  # Initialising the refsite dataframe
  refsite <- epiuf::RefSite
  
  if(missing(ID)) {
    stop("Please provide getRefSiteLocName() with ID to search for")
  }
  
  # Search based on ID
  if(!missing(ID)) {
    results <- unique(refsite[refsite$ID %in% ID, ])
  }
  
  if(nrow(results) == 1) {
    return(results$LocationName)
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