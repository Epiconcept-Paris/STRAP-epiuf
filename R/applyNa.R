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


#' Title
#'
#' @param data 
#' @param varname 
#' @param searchlist 
#' @param join 
#'
#' @return
#' @export
#'
#'
applyNA <- function(data, varname, searchlist=NULL, join=FALSE){
  
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
  
  if (is.character(data[,varname])){
    if (is.null(searchlist)){
      searchlist <- "^[[:space:]]*$|^dnk$|^nd$|^na$|^nsp$|^ne zna$|^unknown$|^do not know$"     # default searchlist if none provided
    }else{
      searchlist <- ifelse(join==TRUE,paste0(searchlist,"|^[[:space:]]*$|^dnk$|^nd$|^na$|^nsp$|^ne zna$|^unknown$|^do not know$"),    # add default searchlist to provided if specified
                           paste(searchlist, collapse = "|"))                                                                         # If searchlist is c(1,2) converted to -> searchlist = "1|2"
    }
    
    numchange <- length(which(grepl(searchlist, data[,varname] )))   # retrieve number of instances which match search list
    
    data[,varname] <- sapply(X = data[,varname],FUN = function(x)
      gsub(pattern = searchlist, x = x, replacement = NA,ignore.case = TRUE))  # for variable specified, replace all instances of searchlist with NA
    
    catret(paste0(varname, ": ", numchange, " values converted to NA"))            # print number of changes made
    
  }
  else if(!is.null(searchlist)){
    numchange <- length(which(grepl(paste(searchlist, collapse = "|"), data[,varname] )))   # retrieve number of instances which match search list
    
    for(i in searchlist){
      data[,varname] <- ifelse(data[,varname]==i, NA, data[,varname])
    }
    catret(paste0(varname, ": ", numchange, " values converted to NA"))            # print number of changes made
  }
  
  return(data[,varname])
  
}

# END of SCRIPT  --------------------------------------------------------

