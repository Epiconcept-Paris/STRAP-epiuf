
#' Create a new secret in default Keyring  
#'
#' This function creates a new secret with a specified keyring ID. 
#' If the keyring already exists, it prompts to use ModifyKeyring instead.
#' Notice: the keyringid is postfixed with _epiufkeyring to avoid conflicts
#'
#' @param keyring_id A string specifying the keyring ID.
#' @param secret A secret string to be stored. It is not recommended to use this parameters because in that case your secret 
#'     will be visible in your code. This parameters should be used only for tests  
#' @return A string with the keyring password.
#' @export
#' 
createKeyring <- function(keyring_id , secret = NULL) {
  if(existsKeyring(keyring_id)) {
    warning("Keyring Exist. Use modifyKeyring() to change it.")
    password <- "" 
  } else {
    message("Keyring not found. Beginning the process of creating a new keyring starting with the API secret value to be stored.")
    if(is.null(secret)) {
      secret <- readline("Enter the actual API key/secret to be stored: ")
      }
    password <- storeKeyring(keyring_id, secret)
  }
  return(password) #LMC 2023-12-21: Do we need to return the password or a message coul dbe good enough?
}

#' Modify an existing keyring
#'
#' This function modifies an existing keyring identified by the keyring ID. 
#' If the keyring is not found, it shows a warning.
#' Notice: the keyringid is postfixed with _epiufkeyring to avoid conflicts
#'
#' @param keyring_id A string specifying the keyring ID.
#' @param secret A secret string to be stored. It is not recommended to use this parameters because in that case your secret 
#'     will be visible in your code. This parameters should be used only for tests  
#' @importFrom keyring keyring_list
#' @importFrom keyring key_delete
#' @return A string with the keyring password or an empty string if keyring is not found.
#' @export
modifyKeyring <- function(keyring_id, secret = NULL) {
  if(existsKeyring(keyring_id)) {
    message("Keyring found. Beginning the process of modifying a keyring with the new API secret value to be stored.")
    if(is.null(secret)) {
      secret <- readline("Enter the actual API key/secret to be stored: ")
      }
    keyring::key_delete(epiufKeyring(keyring_id))
    password <- storeKeyring(keyring_id, secret)
  } else {
    warning("Keyring not found.")
    password <- ""
  }
  return(password)
}

#' Retrieve password from a keyring
#'
#' This function retrieves the password from a specified keyring. 
#' If the keyring does not exist, it issues a warning.
#'
#' @param keyring_id A string specifying the keyring ID.
#' @return A string with the keyring password or an empty string if keyring is not found.
#' @importFrom keyring key_get
#' 
#' @export
grabKeyring <- function(keyring_id) {
  if(existsKeyring(keyring_id)) {
    password <- keyring::key_get(epiufKeyring(keyring_id))
  } else {
    warning("Keyring not found.")
    password <- ""
  }
  return(password)
}

#' Delete  a Keyring
#'
#' This function checks if the specified keyring exists, and if so delete it.
#'
#' @param keyring_id A character string specifying the keyring name to delete
#' @return A character string containing the password for further API calls.
#' @importFrom keyring key_delete
#' @examples
#' \dontrun{
#' # Set a secret for a fictional country called 'DB'
#' deleteKeyring("DB")
#'}
#' @export
deleteKeyring <- function(keyring_id) {
  if(existsKeyring(keyring_id)) {
    keyring::key_delete(epiufKeyring(keyring_id))
    message("Secret deleted")
  } else {
    warning("That keyring wasn't found")
  }
}

#' Delete all specific keyrings
#'
#' This function deletes all keyrings that are identified by the `listKeyring` function. 
#' It iterates over each keyring returned by `listKeyring` and deletes them.
#' 
#' @export
#' @examples
#' # To delete all keyrings identified by `listKeyring` function
#' deleteKeyringAll()
deleteKeyringAll <- function() {
  keys <- listKeyring()
  num <- length(keys)
  if(num > 0) {
    for (i in 1:num) {
      keyring::key_delete(keys[i])
    } 
  }  
  message(catret(num,"Secret deleted"))
}

#' List specific keyrings
#'
#' This function lists all keyrings that contain the term pattern (default to "epiufkeyring") in their names. 
#' It uses a regular expression to find matches and returns the filtered list.
#' @param pattern A pattern to filter the list 
#' @return A character vector of keyring names containing "epiufkeyring".
#' @importFrom keyring key_list
#'  
#' @export
#' @examples
#' # Assuming you have keyrings named 'epiufkeyring_1', 'epiufkeyring_2', etc.
#' listed_keyrings <- listKeyring()
#' print(listed_keyrings)
listKeyring <- function(pattern=NULL) {
  out <- keyring::key_list()$service
  if(is.null(pattern)) {
    pattern <- "epiufkeyring"
    }
  indice <- grep(pattern, out, useBytes = TRUE)
  return(out[indice])
}

#' Generate a standardized keyring ID
#'
#' This function appends "_epiufkeyring" to the given keyring ID to generate a standardized keyring ID.
#' It's useful for maintaining consistency in naming keyrings within your system.
#'
#' @param keyring_id A string specifying the base keyring ID.
#' @return A string representing the standardized keyring ID.
epiufKeyring <- function(keyring_id) {
  return(paste0(keyring_id, "_epiufkeyring"))
}


#' Store a secret in a new keyring
#'
#' This function creates a new keyring and stores a provided secret in it.
#'
#' @param keyring_id A string specifying the keyring ID.
#' @param secret A string representing the secret to be stored in the keyring.
#' @importFrom keyring key_set_with_value
#' @importFrom keyring key_get
#' @return A string with the stored secret.
storeKeyring <- function(keyring_id, secret) {
  trueId <- epiufKeyring(keyring_id) 
  keyring::key_set_with_value(trueId, password = secret)
  password <- keyring::key_get(trueId)
  return(password)
}  

#' Check if a specific keyring exists
#'
#' This function checks for the existence of a keyring. It first standardizes the keyring ID 
#' using `epiufKeyring` function and then checks if this standardized ID is present in the list of keyrings.
#'
#' @param keyring_id A string specifying the base keyring ID.
#' @return A logical value; `TRUE` if the keyring exists, otherwise `FALSE`.
#' @importFrom keyring key_list
#' @export
#' @examples
#' # Check if a keyring exists
#' keyring_exists <- existsKeyring("myKeyring")
#' print(keyring_exists) # Output: TRUE or FALSE, depending on existence
existsKeyring <- function(keyring_id) {
  trueId <- epiufKeyring(keyring_id)
  return(trueId %in% keyring::key_list()$service)
}
