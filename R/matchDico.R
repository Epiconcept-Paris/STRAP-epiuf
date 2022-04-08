#
# Project Name : STRAP
# Script Name  : matchDico
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of matchDico function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to compare variable contents to associated dico and set to NA 
# all those which do not match.
# 

# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

# # NEED TO set config incase have not already (run off main script)
# 
# # import source data for checking
# global$COUNTRY = "HR"
# global$COUNTRYFILE = paste0(global$COUNTRY, " export.csv")
# 
# sourceFile("SCRIPTS","import.R")
# 
# 
# # Import dico
# dicos <- readData(pathToFile("REFERENCE","sari-vebis-VE-dictionary-HR.xlsx"),sheet="dicos")


## matchDico function: 
#' matchDico
#'
#' @param data The dataset to process
#' @param mydicos The dico (epiuf structure) to use 
#' @param varname The variable to check
#' @param diconame Name of the corresponding dico 
#'
#' @return A dataset with potential differences 
#' @export
#'

matchDico <- function(data, mydicos, varname,diconame) {
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  s_op <- deparse(substitute(diconame))
  tryCatch(
    if (is.character(diconame)) {
      s_op <- diconame
    }
    , error = function(c) { }
  )
  diconame <- s_op
  
  
  myDicoCode <- subset(mydicos, mydicos$dico_name==diconame)[,c("code")]
  
  data[,varname] <- ifelse(data[,varname] %in% myDicoCode, data[,varname], NA)
  
}

# 
# 
# # Test code using forced error variable
# 
# # Create new example variable with a forced error
# df$AgeGp2 <- ifelse(df$AgeGp==7, 9, df$AgeGp)
# 
# # veiw variable contents
# tab(AgeGp2)
# 
# # run function
# df$AgeGp2 <- matchDico(df, dicos, AgeGp2, agegp)
# 
# # check output
# tab(AgeGp2)


# END config.R script -----------------------------------------------------
