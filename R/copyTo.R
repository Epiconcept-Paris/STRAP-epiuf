#
# Project Name : epiuf
# Script Name  : copyTo
# GitHub repo  : epiuf
# Summary      : copies files from one location to another, with options to commit, pull and push to github
# Date created : 4/3/24
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------
#' copyTo
#' 
#' Description
#' Wrapper function for file.copy which takes list of files specified and copies
#' to a desired folder. 
#' For use when making backups of file: if date_stamp is 
#' TRUE, a text document is created in the specified folder called 
#' 'date_last_copy.txt' with todays date written inside. 
#' For use when saving copied files to a local github reposity: options to make a comit
#' and pull from then push to github after copying the files.
#' Utility in data management: retrieve refernce documents used by scripts which 
#' are external to github and create a backup within the github repository.
#' 
#' @param files_to_copy List of filepaths for all files to be copied
#' @param desitation Filepath for where you want to save the copied files
#' @param date_stamp Logical - do you want to create a date of backup marker in reference backup folder
#' @param commit Logical - do you want to make a commit of your repository before extraction?
#' @param commit_msg If commit, what is your message. Default is: commit pre zip (todays date)
#' @param pull Logical - do you want to make a pull from github before extraction?
#' @param push Logical - do you want to make a push to github before extraction?

#' @returns
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' 
#' 
copyTo <- function(files_to_copy,destination,date_stamp=TRUE,commit=FALSE,commit_msg=NULL,pull=FALSE,push=FALSE){
  # Copy all files to folder
  results <- file.copy(from = files_to_copy
                       , to = destination
                       ,overwrite = TRUE)
  # Create date of backup marker in reference backup folder
  if(date_stamp){
    writeLines(as.character(Sys.Date()), file.path(destination, "date_last_copy.txt"))
  }
  # Output message for what saved where
  if(any(results)){
    sucessmsg <- c("The following files were saved:",paste0("\n",files_to_copy[results]), "\nto ", destination, "\non ", as.character(Sys.Date()))
  }else{sucessmsg <- ""}
  if(any(results==FALSE)){
    failmsg <- c("The following files failed to save:",paste0("\n",files_to_copy[!results]))
  }else{failmsg <- ""}
  message(sucessmsg,"\n",failmsg)
  # Commit changes
  if(commit){
    if(is.null(commit_msg)){
      commit_msg <- paste0("files saved ", as.character(Sys.Date()))
    }
    files_to_stage <- list.files(destination)
    for (file in files_to_stage) {
      system(paste0("git add -f ", destination,"/",file))
    }
    system(paste0("git commit -m ",shQuote(commit_msg)))
  }
  # send to github if specified
  if(pull){
    system("git pull")
  }
  if(push){
    system("git push")
  }
}


# END of SCRIPT  --------------------------------------------------------