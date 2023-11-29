#' Set or get a file path for REDCap data downloads
#'
#' This function walks the user through setting the filepath that can be called automatically when downloading REDCap data
#'
#' @param country A country code for a REDCap country project
#' @return The file path within which downloaded REDCap data will be placed
#' @examples
#' \dontrun{
#' accessREDCap("DB")
#' # If file path is not found for this country, setOrGetFilePath will then run
#'}
#' @export
setOrGetFilePath <- function(country){
  
  print("Setting the file path can be done in several ways, depending on your project filing practices:") 
  print("1) These data always go to the exact same folder (e.g., my_files/sources)")
  print("2) Data are separated by country name within a shared parent folder (e.g., my_files/sources/spain)")
  print("3) Data are separated by country name AND date of download, within a shared parent folder (e.g., my_files/sources/spain/2023 11)")
  
  file_procedure <- readline("Type which of 1, 2, or 3 best describes your filing practices:")
  
  if(file_procedure == 1){
    file_path <- readline("Paste the full file path either as ID (must already be set with setPath(), e.g., 'SOURCES') or copy paste the full filepath whole:")
    if(!is.null(getPath(file_path))){
      file_path <- getPath(file_path)
    } else {
      print("found no setPath() call, assuming whole file path copy pasted")
    }
  } else if(file_procedure == 2| file_procedure == 3){
    file_path <- readline("Paste the full PARENT folder path either as ID (must already be set with setPath(), e.g., SOURCES) or
                          copy paste the full filepath whole:")
    if(!is.null(getPath(file_path))){
      file_path <- getPath(file_path)
      file_path <- paste0(file_path,country,"/")
    } else {
      print("found no setPath() call, assuming whole file path copy pasted")
      file_path <- paste0(file_path,"/",country,"/")
    }
    
  } else {
    print("Unrecognised input")
  }
  
  print(paste("Does this look like the correct filepath?",file_path))
  answer <- readline("(Y)es, (N)o:")
  
  if(tolower(answer)=="n"|tolower(answer)=="no"){
    print("Starting again.")
  } else {
    if(file_procedure == 1 | file_procedure == 2){
      countrypath_key <- paste0(country,"_pathkeyfull")
    } else if(file_procedure == 3){
      countrypath_key <- paste0(country,"_pathkeypart")
    }
    keyring::keyring_create(countrypath_key,password = country)
    keyring::key_set_with_value(country, keyring = countrypath_key,password = file_path)
    file_path <- keyring::key_get(country, keyring = countrypath_key)
  } 
}