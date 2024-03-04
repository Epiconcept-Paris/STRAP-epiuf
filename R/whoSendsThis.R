#
# Project Name : epiuf
# Script Name  : whoSendsThis
# GitHub repo  : epiuf
# Summary      : Looks for text strings in dictionary column source_name and generic_name
# Date created : 4/3/24
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


# END of SCRIPT  --------------------------------------------------------

#' whoSendsThis
#' 
#' Description
#' The aim of this function to is to quickly ascertain which dictionaries- in a
#' specified folder contain your searchterm.
#' 
#' This function looks into the specified file location, and then opens each dictionary
#' in turn as specified by the dictionary_root_filename and filetags. Once opened it
#' performs a grep search for the searchterm in the source_name and the generic_name
#' colunms and lists all matching variablenames in a table. 
#' 
#' 
#' @param searchterm The character string which wish to look
#' @param filetags The list of dictionary filename endings to search through
#' @param dictionary_location file path to where all the dictionaries that you want to search through are stored
#' @param dictionary_root_filename root filename for all the dictionaries that wish to search through, which change only by the filetag at the end
#' @returns data.frame
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' 
whoSendsThis <- function(searchterm
                         , filetags
                         , dictionary_location
                         , dictionary_root_filename){
  
  # Make table to hold information
  whohasit <- data.frame()
  
  # for each country
  for (ft in filetags){
    # pull in dictionary
    openDictionary(paste0(dictionary_location,"/",dictionary_root_filename,ft,".xlsx"))
    ds <- getDictionary()
    # check source_name
    checkinsourcename <- grep(searchterm, ds$source_name, ignore.case = T)
    if(length(checkinsourcename>0)){
      insourcename <- paste(ds$source_name[checkinsourcename], collapse=", ")
    }else{insourcename <- ""}
    
    # check generic_name
    checkingenericname <- grep(searchterm, ds$generic_name, ignore.case = T)
    if(length(checkingenericname>0)){
      ingenericname <- paste(ds$generic_name[checkingenericname], collapse=", ")
    }else{ingenericname <- ""}
    
    # addd new row to table
    whohasit <- rbind(whohasit, c(ft, insourcename, ingenericname))
  }
  
  colnames(whohasit) <- c("Site", "Source names", "Generic names")
  return(whohasit)
}
