#' Download REDCap Data
#'
#' This function downloads REDCap data from a specified project and saves it as a CSV file. If no file path has previously been set,
#' it defaults to the working directory of the user
#'
#' @param password A character string representing the API token for the desired REDCap project.
#' @param country A character string representing the country/project
#' @param filepath A character string representing the desired file path in which the downloaded data will be saved.
#' 
#' @return A CSV file containing the REDCap data.
#' @export
#' @importFrom utils write.csv
#' @importFrom httr POST 
#' @examples
#' \dontrun{
#' # Please provide your own API token to use this example
#' # If you haven't already, you can set and retrieve password for REDCap using
#' # the createKeyring() or grabKeyring function. 
#' # Run this function or visit it's vignette in epiuf for more information.
#' 
#' # Begin the download process using another function
#' accessREDCap(country)
#' }
downloadREDCap <- function(password, country, filepath = NULL) {
  # The URL we are requesting data from
  url <- "https://extranet.who.int/edcrc/api/"
  
  # Parameters for the request
  formData <- list(token = password,
                   content = 'record',
                   action = 'export',
                   format = 'csv',
                   type = 'flat',
                   csvDelimiter = '',
                   rawOrLabel = 'raw',
                   rawOrLabelHeaders = 'raw',
                   exportCheckboxLabel = 'true',
                   exportSurveyFields = 'true',
                   exportDataAccessGroups = 'true',
                   returnFormat = 'csv'
  )
  
  # Make the request using httr and POST method
  response <- httr::POST(url, body = formData, encode = "form")
  
  # Convert the httr response content and assign it to the name 'REDCapExtract'
  REDCapExtract <- httr::content(response)
  print(REDCapExtract)
  
  # Prompt user for the file path within which they'd like to save the file
  filename <- paste0(country, "_", Sys.Date(), "_", strftime(Sys.time(), "%H%M"), ".csv") 
  # we should use setPath/getPath here
  if(is.null(filepath)) filepath <- getwd()
  location <- file.path(filepath,filename)
  # Download the r dataframe to CSV
  write.csv(REDCapExtract, location, row.names = FALSE)
  
  message(paste("File downloaded:", location))
}