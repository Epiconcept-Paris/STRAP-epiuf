#
# Project Name : STRAP
# Script Name  : applydictionary
# GitHub repo  : https://github.com/Epiconcept-Paris/STRAP-epiuf
# Summary      : editing of applyDictionary function
# Date created : 01/03/2022
# Author       : JHD - GDE
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
# epiDictionaryFiles environement used to manage global values
epidictionaryfiles_env <- new.env(parent = emptyenv())

epidictionaryfiles_env$data <- NULL
epidictionaryfiles_env$datafilename <- NULL
epidictionaryfiles_env$dicos <- NULL
epidictionaryfiles_env$actions <- NULL


# this function would be rarely used except for tests
#' setDictionary
#'
#' @param dictionary A dictionary epiuf structure 
#'
#' @return Nothing
#' @export
#'

setDictionary  <- function(dictionary) {
  epidictionaryfiles_env$data <- dictionary
  epidictionaryfiles_env$data <- updateDataset(epidictionaryfiles_env$data,getNewDictionaryLine("dictionary"))
  
}

#' getDictionary
#'
#' @return The current dictionary
#' @export
#'
getDictionary <- function() {
  return(epidictionaryfiles_env$data)
}

#' openDictionary
#'
#' @param filename The file (xls) containing a dictionary. The dictionary will be loaded
#'
#' @return Nothing
#' @export
#'

openDictionary <-  function(filename) {
  epidictionaryfiles_env$datafilename <- filename
  
  # need more checks to verify that sheet exists with good name ! 
  if (file.exists(filename)) {
    epidictionaryfiles_env$data <- readData(filename,sheet="dictionary")
    # we need to update structure if needed
    epidictionaryfiles_env$data <- updateDataset(epidictionaryfiles_env$data,getNewDictionaryLine("dictionary"))
    
    
    # if multivarname is a variable which contain char, we use content of multivarname
    tryCatch(
      epidictionaryfiles_env$dicos <- readData(filename,sheet="dicos")
      , error = function(c) { 
        epidictionaryfiles_env$dicos <- getNewDictionaryLine("dicos")
        }
    )
    epidictionaryfiles_env$dicos <- updateDataset(epidictionaryfiles_env$dicos,getNewDictionaryLine("dicos"))
    
    tryCatch(
        epidictionaryfiles_env$actions <- readData(filename,sheet="actions")
      , error = function(c) { 
        epidictionaryfiles_env$actions <- getNewDictionaryLine("actions")
      }
    )
    
    epidictionaryfiles_env$actions <- updateDataset(epidictionaryfiles_env$actions,getNewDictionaryLine("actions"))
    
  } else {   # datadictionary doesn't exist we have to create it
    red("Datadictionary ",filename," not found. Empty dictionary created")
    # we need to create the 3 data sheet
    epidictionaryfiles_env$data <- getNewDictionaryLine("dictionary")
    epidictionaryfiles_env$dicos <- getNewDictionaryLine("dicos")
    epidictionaryfiles_env$actions <- getNewDictionaryLine("actions")
  }
}


# may be only useful to create an empty dictionary, not urgent 
#' saveDictionary
#'
#' @param filename The filename where to save the dictionary. 
#' This file will be erased and replaced by a new  one containing the \code{dictionary} 
#' @param dictionary Optionaly a dictionary. By default the current dictionary will be saved
#'
#' @return Nothing
#' @export
#'

saveDictionary <- function(filename=NULL,dictionary=NULL) {
  # checks to be added in case dataset doesn't exists in memory GDE ? 
  if (is.null(filename)) {
    filename <- epidictionaryfiles_env$datafilename
    if (is.null(filename)) {
      # filename <- pathToFile("SOURCES","datasources.xls")
    }
  }
  if(is.null(dictionary)) {
    ds <- getDictionary()
  } else ds <- dictionary
  
  if (nrow(ds)==0) {
    # replace by empty record ? 
    ds[1,] <- " "
  }
  xlsx::write.xlsx(ds,file=filename,sheetName = "dictionary",row.names=FALSE)
  # to add some check ?
  ds <- epidictionaryfiles_env$dicos
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode="dicos")
    ds[1,] <- NA
  }
  xlsx::write.xlsx(ds,file=filename,sheetName = "dicos",append=TRUE,row.names=FALSE)
  
  ds <- epidictionaryfiles_env$actions
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode="actions") 
    ds[1,] <- NA
  }
  xlsx::write.xlsx(ds,file=filename,sheetName = "actions",append=TRUE,row.names=FALSE)
  
}

# we need to add the current status ! 
#' getNewDictionaryLine
#'
#' @param mode Type of line to return. Could be dictionary, dicos, or actions
#'
#' @return An empty record of type \code{mode}
#' @export
#'

getNewDictionaryLine  <- function(mode="dictionary") {

  if (mode=="dictionary") {  
  OneDataLine <- data.frame(source_name=as.character(),               # name in the source
                            generic_name=as.character(),              # generic name replacement 
                            type=as.character(),                      # type of the variable
                            dico=as.character(),
                            unknowns=as.character(),
                            description=as.character(),
                            comments=as.character(),
                            stringsAsFactors=FALSE
                            )
  } else if (mode=="dicos") {
    OneDataLine <- data.frame(dico_name=as.character(),               # name in the source
                              label=as.character(),              # generic name replacement 
                              code=as.character(),
                              stringsAsFactors=FALSE
                              )
                              
  } else if (mode=="actions") {
    OneDataLine <- data.frame(variable=as.character(),               # name in the source
                              action_group=as.character(),              # generic name replacement 
                              parameters=as.character(),
                              stringsAsFactors=FALSE
                              )
                              
  }
  return(OneDataLine)
}


#' getDicos
#'
#' @return The dataset containing all the dicos 
#' @export
#'

getDicos <- function() {
  return(epidictionaryfiles_env$dicos)
}

#' setDicos
#'
#' @param dic A dataset of dicos (with epiuf structure as returned by getNewDictionaryLine)
#'
#' @return nothing
#' @export
#'
#'  
setDicos <- function(dic) {
  epidictionaryfiles_env$dicos <- dic
}

#' getDictionaryActions
#'
#' @return A dataset of actions 
#' @export
#'
#'  
getDictionaryActions <- function() {
  return(epidictionaryfiles_env$actions)
}

#' setDictionaryActions
#'
#' @param actions A dataset of actions 
#'
#' @return Nothing
#' @export
#'
#'  
setDictionaryActions <- function(actions) {
  epidictionaryfiles_env$actions <- actions
}


#' getDictionaryValue
#'
#' @param varname The varname for which we will retrieve content of one column from dictionary
#' @param valuename Name of the coulumn to retrieve from dictionary
#'
#' @return A single value
#' @export
#'
#'  
getDictionaryValue <- function(varname, valuename) {
  ds <- getDictionary()

  if (nrow(ds)>0) {
     value <- subset(ds,ds$generic_name == varname)[,valuename] 
  }  
  return(value)
}

#' getDicoOfVar
#'
#' @param varname The variable for which we want to retrieve the name of the associated dico
#'
#' @return The name of the dico associated with the variable
#' @export
#'
#'  
getDicoOfVar <- function(varname) {
   diconame <- getDictionaryValue(varname,"DICO")
   getDico(diconame)
}

#' getDico
#'
#' @param diconame The name of one dico from the dicos structure
#'
#' @return A datset containing one dico (list of code/labels)
#' @export
#'
#'  
getDico <- function(diconame) {
  ds <- getDicos()
  ds <-  subset(ds,ds$dico == diconame)
}

#' getVarAction
#'
#' @param variablename The variable for which we want to retrieve the the associated action
#' @param actiontag The name of the action group to retrieve
#'
#' @return A dataset of var actions records for the variable 
#' @export
#'
#'  
getVarAction <- function(variablename,actiontag) {
  ds <- getDictionaryActions()
# GDE check to be added for wrong action name
    ds <-  subset(ds,ds$variable == variablename & ds$action_group == actiontag )
}

#' getVarActionParameters
#'
#' @param variablename The variable for which we want to retrieve the the associated action parameters
#' @param actiontag Name of the action group 
#'
#' @return The parameters associated to the variable action
#' @export
#'
#'  
getVarActionParameters <- function(variablename,actiontag) {
  ds <- getVarAction(variablename,actiontag)
  ifelse(nrow(ds)>0, ds$parameters, NA)
}  

#' getActionGroup
#'
#' @param actiontag Name of the action group to retrieve
#'
#' @return dataset containing all the variables with actions of type actionname
#' @export
#'
#'  
getActionGroup <- function(actiontag) {
  ds <- getDictionaryActions()
  ds <-  subset(ds, ds$action_group == actiontag )
}



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

applyDictionary <- function( dictionary=NULL, data, verbose=TRUE, keepextra = FALSE) {
  
  if (is.null(dictionary)) {
     dictionary <-  getDictionary()
  } 
  # this function get non NA/Empty content of one column (mainly to get varname)
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
  
  if (verbose==TRUE) {
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
  if (verbose==TRUE) {
    epiuf::bold("Vars not in generic and dropped  : ", length(todrop))
    catret()
    catret(sort(todrop),sep="  \n")
  } 
  
  # remove from the generic dataset the variables to be dropped because not defined in dictionnary
  gen <- gen[, -which( (names(gen) %in% todrop) )]
  
  
  MatchNames <- MatchNames[! is.na(MatchNames[dicGenericName]),]
  
  if (verbose==TRUE) {
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
  if (verbose==TRUE) {
    bold("Generic vars created (as empty) : ", length(VarMiss))
    catret()
    catret(sort(VarMiss), sep=", ")
  }
  
  gen
}


#' createDictionary
#'
#' @return An empty dictionnary
#' @export
#'
#'  
createDictionary <- function() {
  # base 
  dic <- data.frame(generic_name=character(),
                    source_name=character(),
                    type=character(),
                    dico=character(),
                    description=character(),
                    comment=character(),
                    unknown=character())
  
} 



# END of SCRIPT  --------------------------------------------------------
