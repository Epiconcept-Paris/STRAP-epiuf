#' Create or Grab a Keyring
#'
#' This function checks if the specified keyring exists, and if not, it creates a new keyring with the provided password.
#' If the keyring exists, it uses the existing password for further API calls.
#'
#' @param keyring_id A character string specifying the keyring name to check or create.
#' @return A character string containing the password for further API calls.
#' @examples
#' \dontrun{
#' # Set a secret for a fictional country called 'DB'
#' CreateOrGrabKeyring("DB")
#'}
#' @export
CreateOrGrabKeyring <- function(keyring_id) {
  key_id <- paste0(keyring_id,"_keyring")
  if(key_id %in% keyring::keyring_list()$keyring) {
    keyring::keyring_unlock(keyring = key_id, password = keyring_id)
    password <- keyring::key_get(keyring_id, keyring = key_id)
  } else {
    print("Keyring not found. Beginning the process of creating a new keyring starting with the API secret value to be stored.")
    secret <- readline("Enter the actual API key/secret to be stored: ")
    keyring::keyring_create(key_id,password = keyring_id)
    keyring::key_set_with_value(keyring_id, keyring = key_id,password = secret)
    password <- keyring::key_get(keyring_id, keyring = key_id)
  }
  return(password)
}