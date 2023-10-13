#' Create or Update a Keyring
#'
#' This function checks if the specified keyring exists, and if not, it creates a new keyring with the provided password.
#' If the keyring exists, it uses the existing password for further API calls.
#'
#' @param keyring_id A character string specifying the keyring name to check or create.
#' @return A character string containing the password for further API calls.
#' @examples
#' \dontrun{
#' keyring_id <- "my_new_keyring"
#' password <- createOrUpdateKeyring(keyring_id)
#' print(password)
#'}
#' @export
createOrUpdateKeyring <- function(keyring_id) {
  if(keyring_id %in% keyring::keyring_list()$keyring) {
    print(paste0("Keyring '", keyring_id, "' already exists. Using existing password for API call."))
    password <- Sys.getenv(keyring_id)
    print(password)
  } else {
    password <- readline(prompt = paste0("Keyring '", keyring_id, "' does not exist. Please enter a password for this keyring: "))
    keyring::keyring_create(keyring_id, password = password)
    print(paste0("Keyring '", keyring_id, "' has been created."))
  }
  return(password)
}