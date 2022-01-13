#
# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
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

# library loading function which ask for confirmation before installing library
loadLib <- function(libname) {
  if (!require(libname, character.only = TRUE)) {
    cat("Required lib ",libname," is required but not installed")
    rep <- readline("Would you like to install it ? Y/N :")
    if (toupper(rep)=="Y")  {
      install.packages(libname, dependencies = TRUE)
      library(libname, character.only = TRUE)
    }  
    else {
      stop("Some Library have to be installed, check requirements")
    }
  }
  return(as.character(libname))
}



# END of SCRIPT  --------------------------------------------------------