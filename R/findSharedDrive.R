# Project Name : epiuf
# Script Name  : findSharedDrive.R
# GitHub repo  : STRAP-epiuf
# Summary      : 
# Date created : 2024-05-23
# Author       : Lore Merdrignac
# Date reviewed:
# Reviewed by  : 

# Description -------------------------------------------------------------

# Changes Log -------------------------------------------------------------


# START of SCRIPT  --------------------------------------------------------


#' findSharedDrive
#' 
#' Quick way to find the path to the root Shared Drive (e.g., "G:/Shared drive"). 
#' It will identify the letter associated to the Drive (e.g., D:, G:, etc) 
#' and then the name of the root folder (i.e., "Shared drive" or "Drive partagé").
#' Indeed, depending on the user language, it could be labelled in english or French.
#' Only these tow languages are implemented so far.
#' 
#' @param message boolean to print information messages or not
#' @returns path to the shared drive
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @examples
#' setPath("DRIVE", findSharedDrive())
#' getPath("DRIVE")
#' 

findSharedDrive <- function(message = FALSE) {
  
  # Get the list of drives
  os <- Sys.info()["sysname"]
  
  if (os == 'Windows') {
    
    # Windows command to get drives
    drives <- system("wmic logicaldisk get caption,description", intern = TRUE)
    # Remove the header line
    drives <- drives[-1]
    # Split the drive information into columns
    drives_data <- strsplit(drives, "\\s+")
    drives_data <- lapply(drives_data, function(x) x[x != ""])
    drives_data <- do.call(rbind, drives_data)
    # Extract the drive letters and names
    drive_letters <- drives_data[, 1]
    
  } else if (os == 'Darwin') {
    
    # macOS command to get drives
    drives <- system("df -h | grep '^/dev/' | awk '{print $1}'", intern = TRUE)
    # Extract drive letters
    drive_letters <- drives
    
  } else if (os == 'Linux') {
    
    # Linux command to get drives
    drives <- system("lsblk -o NAME,MOUNTPOINT | grep '^sd' | awk '{print $1}'", intern = TRUE)
    # Extract drive letters
    drive_letters <- drives
    
  } else {
    stop("Unsupported operating system")
  }
  

  # Initialising the result to return
  result <- NULL
  
  # Searching among the drives if Shared drive or Drive partag\u00E9s exist and store it
  for (i in seq_along(drive_letters)) {
    
    # Add 'Shared drives' to the drive letter and list files
    shared_drives_path <- file.path(drive_letters[i], "Shared drives")
    
    if (dir.exists(shared_drives_path)) {
      result <- shared_drives_path
      if (message == TRUE) message(paste("Shared drive found here:", result))
      
    } else {
      
      # Add 'Shared drives' to the drive letter and list files
      shared_drives_path <- file.path(drive_letters[i], "Drive partag\u00E9s")
      
      if (dir.exists(shared_drives_path)) {
        result <- shared_drives_path
        if (message == TRUE) message(paste("Shared drive found here:", result))
      }
    }
  }
  
  return(result)
  
}
# END of SCRIPT  ----------------------------------------------------------
