#' Download REDCap Data
#'
#' This function downloads REDCap data from a specified project and saves it as a CSV file.
#'
#' @param password A character string representing the API token for the desired REDCap project.
#' 
#' @return A CSV file containing the REDCap data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Please provide your own API token to use this example
#' # If you haven't already, you can set and retrieve password for REDCap using the accessREDCap() function. Run this function or visit it's vignette in epiuf for more information.
#' api_token <- "your_api_token_here"
#' 
#' # Download REDCap data using the specified API token
#' downloadRedCap(api_token)
#' }
downloadRedCap <- function(password) {
  
  # The URL we are requesting data from
  url <- "https://extranet.who.int/edcrc/api/"
  
  # Parameters for the request
  formData <- list(token=password,
                   content='record',
                   action='export',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='true',
                   exportSurveyFields='true',
                   exportDataAccessGroups='true',
                   returnFormat='csv'
  )
  
  # Make the request using httr and POST method
  response <- httr::POST(url, body = formData, encode = "form")
  
  # Convert the httr response content and assign it to the name 'REDCapExtract'
  REDCapExtract <- httr::content(response)
  print(REDCapExtract)
  
  # Prompt user for the file path within which they'd like to save the file
  dataLocation <- readline(prompt = "Paste the file path within which you'd like to save this file")
  fileName <- paste0(dataLocation, Sys.Date(), "_", strftime(Sys.time(), "%H%M"), ".csv")
  
  # Download the r dataframe to CSV
  write.csv(REDCapExtract, fileName, row.names=FALSE)
}