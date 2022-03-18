#
# Project Name : STRAP
# Script Name  : checkDico
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of checkDico function
# Date created : 25/02/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  : GDE

# Description --------------------------------------------------------------
# Function aims to check codes in source or generic dataset against the data 
# dictionary. Currently output is variable code and dicos code list for all  
# variables where source or generic data has extra code (unless variable is empty).
# For checkbox variables, 




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

# NEED TO set config incase have not already (run off main script)
# 
# # import source data for checking
# global$COUNTRY = "HR"
# global$COUNTRYFILE = paste0(global$COUNTRY, " export.csv")
# 
# sourceFile("SCRIPTS","import.R")
# 
# # Import dictionaries
# dictionary <- readData(pathToFile("REFERENCE","sari-vebis-VE-dictionary-HR.xlsx"))
# dicos <- readData(pathToFile("REFERENCE","sari-vebis-VE-dictionary-HR.xlsx"),sheet="dicos")

# Function: 

checkDico <- function(data, mydictionary, mydicos, datatype) {
  
  # Pint header message
  
  catret()
  bold("Variable code and dico code if mismatched:")
  catret()
  
  s_op <- deparse(substitute(datatype))
  # if datatype is a variable which contain char, we use content of datatype
  tryCatch(
    if (is.character(datatype)) {
      s_op <- datatype
    }
    , error = function(c) { }
  )
  datatype <- s_op
  
  
  # Define set of variables names to be used (depending on if want generic or source names)
  # this step is important depending on when the dataset is being checked (pre or post recode)
  names(mydictionary)[names(mydictionary) == paste0(datatype, "_name")]  <- "temp_name"
  
  # Select all those variables names in temp name that have a mydictionary code (includes checkboxes and factors)
  dicovar <- subset(mydictionary, !is.na(mydictionary$dico)&!is.na(mydictionary$temp_name))$temp_name
  
  # identify all empty vectors
  dicovarempty <- sapply(dicovar, function(x)all(is.na(data[,x])))
  
  # remove from list of variables
  dicovar <- names(which(!dicovarempty))
  
  
  for (d in dicovar){
    diconame <- subset(mydictionary, mydictionary$temp_name==d)$dico
    
    myDicoCode <- c(subset(mydicos, mydicos$dico_name==diconame)[,"code"], NA)
    
    # If factor with single inputs can compute directly the differences
    if (subset(mydictionary, mydictionary$temp_name==d)[,"type"]=="factor"){
    dataNotDico <- setdiff(data[,d], myDicoCode)
    }
    
    # If checkbox, then must separate out multiple inputs to find list of unique numbers present to check
    else if (subset(mydictionary, mydictionary$temp_name==d)[,"type"]=="checkbox"){
    
      checkUnique <- unique(as.numeric(unlist(regmatches(myDicoCode[, d], gregexpr("[[:digit:]]+", myDicoCode[, d])))))
      dataNotDico <- setdiff(checkUnique, myDicoCode)
    }
    
    if (length(dataNotDico)>0) {
        catret(d, ":" , unique(data[,d]))
        catret(diconame, ":", myDicoCode)
        catret()
    }
  }
  
  # reset variable name to original
  names(mydictionary)[names(mydictionary) == "temp_name"]  <- paste0(datatype, "_name") 
  
}

# # test
# checkDico(df, dictionary, dicos, source)




# END of SCRIPT  --------------------------------------------------------

