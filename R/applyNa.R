#
# Project Name : STRAP
# Script Name  : applyNa
# GitHub repo  : STRAP-Common-Tasks
# Summary      : construction fo the applyNa function
# Date created : 15/3/22
# Author       : Jenny and Cristina
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Searches a variable for specified parameters and replaces with NA


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


#' applyNA 
#' 
#'   Default Unknowns : dnk nd na ne zna unknown do not know
#'
#' @param data Dataset which contain the variable to be processed
#' @param varname The variable where all kind of UNKNOWNS should be identified and replaced by NA
#' @param searchlist An optional list of unknown values
#' @param join False by default, if TRUE, the searchlist will be added to the default list of UNKNOWNS 
#'
#' @return variable with NAs edited and message to confirm, or warning   GDE or data ?
#' @export
#'
#'
applyNA <- function(data, varname, searchlist=NULL, join=FALSE){
  onetoreg <- function(x) {
    x <- paste0("^",x,"$")
    return(x)
  } 
  
  # Run code to make it possible to input varname without ""
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  defaultsearch <- "^[[:space:]]*$|^dnk$|^nd$|^nk$|^na$|^nsp$|^ne zna$|^unknown$|^unk$|^do not know$|^ne sait pas$"     # default searchlist if none provided
    
  if (is.character(data[,varname])){
    if (is.null(searchlist)){
      searchlist <- defaultsearch
    }else{
      # if searchlist is a list (and not a character) we unlist 
      searchlist <- paste(unlist(searchlist),collapse=",") 
      # If searchlist is 1,2 converted to -> searchlist = "^1$|^2$"
      # but only if not already contain regex
      regex <-  ifelse (charCount("\\^",searchlist)==0,FALSE,TRUE)
      # we split char into a list 
      searchlist <- strsplit(searchlist,",")
      # we transform as regex if needed 
      if (regex == FALSE)  searchlist <- lapply(searchlist,onetoreg)
      # we transform back to character but with | as separator
      searchlist <- paste(unlist(searchlist), collapse = "|")
      # add default searchlist to provided if specified
      searchlist <- ifelse(join==TRUE,paste0(searchlist,"|",defaultsearch),   
                      searchlist)              
    }
    numchange <- length(which(grepl(searchlist, data[,varname] )))   # retrieve number of instances which match search list
    
    data[,varname] <- gsub(pattern = searchlist, x = data[,varname], replacement = NA,ignore.case = TRUE)  # for variable specified, replace all instances of searchlist with NA
    
    catret(paste0(varname, ": ", numchange, " values converted to NA"))            # print number of changes made
    
  }
  else if(!is.null(searchlist)){
    
    if (is.character(searchlist)) searchlist <- as.numeric(unlist(strsplit(searchlist,",")))
    numchange <- length(which(grepl(paste(searchlist, collapse = "|"), data[,varname] )))   # retrieve number of instances which match search list
    
    for(i in searchlist){
      data[,varname] <- ifelse(data[,varname]==i, NA, data[,varname])
    }
    catret(paste0(varname, ": ", numchange, " values converted to NA"))            # print number of changes made
  }
  
  return(data[,varname])
  
}



# END of SCRIPT  --------------------------------------------------------

