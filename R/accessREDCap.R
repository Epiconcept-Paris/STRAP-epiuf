#' Access REDCap data using a keyring for the API password
#'
#' This function, 'accessREDCap', prompts the user to enter a country code, or project name, that corresponds to a
#' REDCap project. The project name should also be the keyring_ID holding the relevant REDCap API password.
#' If the keyring_ID exists, the password is retrieved and used to query the API.
#' If it does not exist, the user is prompted to enter the password, which is saved as a keyring and used to query the API.
#' Finally, the user is prompted to enter a file path where the returned data frame will be saved.
#'
#' @return (Invisible) The location where the returned data frame is saved.
#' @export
#'
#' @examples
#' \dontrun{
#' accessREDCap()
#' }
accessREDCap <- function() {
  country <- readline("Please type the country code of the REDCap project you'd like to access. Note:
           this must also be the keyring_ID holding the relevant REDCap API password.")
  password <- createOrUpdateKeyring(country)
  downloadRedCap(password)
}