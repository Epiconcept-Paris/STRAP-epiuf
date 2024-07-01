#
# Project Name : epiuf
# Script Name  : zipGithubRepo
# GitHub repo  : epiuf
# Summary      : saves zipped github repo to specified location, option to pull and push before
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

#' zipGithubRepo
#' 
#' Description
#' Make an extraction from your current github repo and saves it to the indicated
#' folder under the indicated name. Options to make a comit and pull from then 
#' push to github before extracting the repository. 
#' 
#' @param zipfile_destination Where you want to save your zipped repository
#' @param zipfile_name What you want to call your saved zipped repository
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
zipGithubRepo <- function(zipfile_destination,zipfile_name,commit=TRUE,commit_msg=NULL,pull=TRUE, push=TRUE){
  # Commit changes
  if(commit){
    if(is.null(commit_msg)){
      commit_msg <- paste0("commit pre zip ", as.character(Sys.Date()))
    }
    system("git add .")
    system(paste0("git commit -m ",shQuote(commit_msg)))
  }
  # send to github if specified
  if(pull){
    system("git pull")
  }
  if(push){
    system("git push")
  }
  # save extraction
  system(paste0("git archive --format=zip --output=",shQuote(paste0(zipfile_destination,"/",zipfile_name,".zip"))," HEAD"), intern=TRUE)
  
  # print message
  message("Copy of ",sub(".*/", "", getwd()), " repository save to:\n"
          ,zipfile_destination,"/",zipfile_name, ".zip")
}


# END of SCRIPT  --------------------------------------------------------