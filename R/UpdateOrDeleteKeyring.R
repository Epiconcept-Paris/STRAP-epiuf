#' Delete or Update a Keyring
#'
#' This function checks if the specified keyring exists, and if so, asks the user whether they'd like to delete or update it.
#'
#' @param keyring_id A character string specifying the keyring name to delete or update
#' @return A character string containing the password for further API calls.
#' @examples
#' \dontrun{
#' # Set a secret for a fictional country called 'DB'
#' UpdateOrDeleteKeyring("DB")
#'}
#' @export
UpdateOrDeleteKeyring <- function(keyring_id) {
  key_id <- paste0(keyring_id,"_keyring")
  if(key_id %in% keyring::keyring_list()$keyring) {
    answer <- readline("The keyring record has been found. Type *D*elete to delete the key, or *U*pdate to update the secret.")
    if(tolower(answer)=="d"|tolower(answer)=="delete"){
      print("Deleting - When prompted, type yes")
      keyring::keyring_delete(key_id)
      print("Secret deleted")
    } else if(tolower(answer)=="u"|tolower(answer)=="update"){
      print("Updating - When prompted, type yes")
      keyring::keyring_delete(key_id)
      CreateOrGrabKeyring(keyring_id)
    } else {
      print("Answer unclear, write *D*elete or *U*pdate only. Starting again")
      UpdateOrDeleteKeyring(keyring_id)
    }
  } else {
    print("That keyring wasn't found. Would you like to create a new keyring? Type (Y)es or (N)o.")
    CreateOrGrabKeyring(keyring_id)
  }
}