#' Access REDCap data for country
#'
#' This function checks for an existing secret key and file path for the specified country.
#' If these exist, the function will proceed to download REDCap data.
#' If not, the user will be prompted to create the necessary credentials.
#'
#' @param country A character string representing the country for which REDCap data will be accessed.
#'
#' @return If a secret key and file path exist, this function will proceed to call `downloadRedCap()` with the appropriate arguments.
#'         Otherwise, it will prompt the user to create a secret key or file path, or to continue with the download without them.
#' 
#' @seealso \code{\link{downloadRedCap}} for downloading REDCap data.
#' @seealso \code{\link{CreateOrGrabKeyring}} for creating or getting a keyring.
#' @seealso \code{\link{setOrGetFilePath}} for setting or getting a file path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Request data for a fictional country 'DB'
#' accessREDCap("DB")
#' }
accessREDCap <- function(country) {
  password <- CreateOrGrabKeyring(country)
  
  country_filepathfull <- paste0(country,"_pathkeyfull")
  country_filepathpartial <- paste0(country,"_pathkeypart")
  
  if(country_filepathfull %in% keyring::keyring_list()$keyring){
    country_filepath <- country_filepathfull
    keyring::keyring_unlock(keyring = country_filepath, password = country)
    file_path <- keyring::key_get(country, keyring = country_filepath)
    
    downloadRedCap(password,country,file_path)
    
  } else if(country_filepathpartial %in% keyring::keyring_list()$keyring){
    country_filepath <- country_filepathpartial
    yearDate <- format(Sys.Date(), "%Y")
    month_number <- format(Sys.Date(), "%m")
    
    keyring::keyring_unlock(keyring = country_filepath, password = country)
    file_path <- keyring::key_get(country, keyring = country_filepath)
    file_path <- pathToFile("SOURCES", paste0(file_path,"/", yearDate ," ", month_number))
    
    downloadRedCap(password,country,file_path)
    
  } else {
    print("A file path, within which the downloaded data will be placed, has not been set for this country yet.") 
    store_file <- readline("Would you like to set one now? It may save time in future. Type (Y)es or (N)o.")
    if(tolower(store_file)=="y"|tolower(store_file)=="yes"){
      setOrGetFilePath(country)
      accessREDCap(country)
    } else {
      downloadRedCap(password,country)
    }
  }
}