#
# Project Name : STRAP
# Script Name  : applydictionary
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : editing of applyDictionary function
# Date created : 01/03/2022
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# applyDictionary function currently not perfectly suited to the recoding tasks
# thus create this temporary extra where I can test edits before feedback to 
# package.
# 
# 


# Changes Log --------------------------------------------------------------
# 115-119 edit generated variable inputs to be NA, and not any values - fixes bug of dates becoming todays date, and 0 getting accidentally included.

# START of SCRIPT  --------------------------------------------------------

#' applyDictionary
#'
#' @param dictionary A dictionary (passed as dataframe)
#' @param data  A dataset to transform to generic
#' @param verbose Should we have feedback 
#' @param keepextra if TRUE, extra variables are keept in generic dataset (no longer generic then...)
#'
#' @return A data set 
#' @export
#'


applyDictionary <- function(dictionary, data, verbose=TRUE, keepextra = FALSE) {
  
  getColValues <- function(dataset, colname) {
    result <- unlist(dataset[ ! (dataset[,colname]=="" ) ,colname])
    result <- result[! is.na(result)]
  }
  
  # Name of columns in dictionary, just here for easier change. Should be a parameters ?  
  dicGenericName <- "generic_name"
  dicSourceName <- "source_name"
  
  # we make a character vector of generic names from dictionary
  NewNames <- getColValues(dictionary,dicGenericName)  
  
  # we make a character vector of sources names from dictionary
  OldNames <- getColValues(dictionary,dicSourceName)
  
  # we make a character vector of sources names from sources
  CurNames <- unlist(names(data))
  
  # variables defined in source_name but missing in the imported dataset comparer to dictionary (dictionary not up to date)
  VarMiss <- setdiff(OldNames,CurNames)
  
  # Extra variables in source that are not defined in dictionary source_name
  VarExtra  <- setdiff(CurNames,OldNames)
  
  if (verbose) {
    epiuf::bold("Vars missing in imported : ", length(VarMiss))
    catret()
    catret(sort(VarMiss),sep="  \n")
  }  
  
  # the generic dataset is created from data taking in account the extra vriable 
  if  (length(VarExtra) >0 ) {
    if (! keepextra ) {
      # we remove the extra column except if exists in generic 
      epiuf::bold("Extra vars in imported (dropped if not exists in generic) : ",length(VarExtra))
      catret()
      catret(sort(VarExtra),sep="  \n")
      gen <- data[, -which( (names(data) %in% VarExtra) & !(names(data)%in%NewNames)  )]
    } else {
      epiuf::bold("Extra vars in imported keept in generic  : ",length(VarExtra))
      catret()
      catret(sort(VarExtra),sep="  \n")
      gen <- data  
    }
  } else { gen <- data } 
  
  # we merge only the matching
  CurNames <- as.data.frame(CurNames)
  MatchNames <-  merge(dictionary,CurNames, by.x=dicSourceName, by.y="CurNames")
  todrop <- MatchNames[is.na(MatchNames[dicGenericName]),dicSourceName]
  if (verbose) {
    epiuf::bold("Vars not in generic and dropped  : ", length(todrop))
    catret()
    catret(sort(todrop),sep="  \n")
  } 
  
  # remove from the generic dataset the variables to be dropped because not defined in dictionnary
  gen <- gen[, -which( (names(gen) %in% todrop) )]
  
  
  MatchNames <- MatchNames[! is.na(MatchNames[dicGenericName]),]
  
  if (verbose) {
    bold("Imported vars renamed with a generic name : ", nrow(MatchNames))
    catret()
    listMatchNames <- list()
    for (i in 1:nrow(MatchNames) ){
      listMatchNames[i] <- paste(MatchNames[i,dicGenericName],"<=",MatchNames[i,dicSourceName])
    }
    catret(unlist(listMatchNames),sep="\n")
    if (nrow(MatchNames) > 20) {
      catret("...")
    }  
  }
  # rename the existing
  for (i in 1:nrow(MatchNames)) {
    thenewname <- MatchNames[i,dicGenericName]
    theoldname <- MatchNames[i,dicSourceName]
    names(gen)[names(gen) == theoldname] <- thenewname 
  }
  
  #now check for missing in final
  CurNames <- names(gen)
  VarMiss <- setdiff(NewNames,CurNames)
  for (i in 1:length(VarMiss) ) {
    typevar <-  dictionary[dictionary[[dicGenericName]]==VarMiss[i],"type"]
    typevar <- typevar[! is.na(typevar)]
    valuevar <- switch (typevar,
                        "numeric" = as.numeric(NA), ## EDIT all to be NA and not a value
                        "character" = as.character(NA),
                        "date" = as.Date(NA),
                        NA
    )
    gen[,VarMiss[i]] <- valuevar
  } 
  if (verbose) {
    bold("Generic vars created (as empty) : ", length(VarMiss))
    catret()
    catret(sort(VarMiss), sep=", ")
  }
  
  gen
}



# END of SCRIPT  --------------------------------------------------------
