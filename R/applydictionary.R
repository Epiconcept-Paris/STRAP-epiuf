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
#' Title
#'
#' @param dictionary 
#'
#' @return
#' @export
#'

setDictionary  <- function(dictionary) {
  epidictionaryfiles_env$data <- dictionary
  epidictionaryfiles_env$data <- updateDataset(epidictionaryfiles_env$data,getNewDictionaryLine("dictionary"))
  
}

#' Title
#'
#' @return
#' @export
#'
getDictionary <- function() {
  return(epidictionaryfiles_env$data)
}

#' Title
#'
#' @param filename 
#'
#' @return
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
#' Title
#'
#' @param filename 
#' @param dictionary 
#'
#' @return
#' @export
#'

saveDictionary <- function(filename=NULL,dictionary=NULL) {
  # checks to be added
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
  if (nrow(ds)==0) {
    # replace by empty record ? 
    ds[1,] <- " "
  }
  xlsx::write.xlsx(ds,file=filename,sheetName = "dicos",append=TRUE,row.names=FALSE)
  
  ds <- epidictionaryfiles_env$actions
  if (nrow(ds)==0) {
    # replace by empty record ? 
    ds[1,] <- " "
  }
  xlsx::write.xlsx(ds,file=filename,sheetName = "actions",append=TRUE,row.names=FALSE)
  
}

# we need to add the current status ! 
#' Title
#'
#' @param mode 
#'
#' @return
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
                              gaction_group=as.character(),              # generic name replacement 
                              parameters=as.character(),
                              stringsAsFactors=FALSE
                              )
                              
  }
  return(OneDataLine)
}


#' Title
#'
#' @return
#' @export
#'

getDicos <- function() {
  return(epidictionaryfiles_env$dicos)
}

#' Title
#'
#' @param dic 
#'
#' @return
#' @export
#'
#'  
setDicos <- function(dic) {
  epidictionaryfiles_env$dicos <- dic
}

#' Title
#'
#' @return
#' @export
#'
#'  
getDictionaryActions <- function() {
  return(epidictionaryfiles_env$actions)
}

#' Title
#'
#' @param actions 
#'
#' @return
#' @export
#'
#'  
setDictionaryActions <- function(actions) {
  epidictionaryfiles_env$actions <- actions
}


#' Title
#'
#' @param varname 
#' @param valuename 
#'
#' @return
#' @export
#'
#'  
getDictionaryValue <- function(varname, valuename) {
  ds <- getDictionary()

  if (nrow(ds)>0) {
     value <- subset(ds,generic_name == varname)[,valuename] 
  }  
  return(value)
}

#' Title
#'
#' @param varname 
#'
#' @return
#' @export
#'
#'  
getDicoOfVar <- function(varname) {
   diconame <- getDictionaryValue(varname,"DICO")
   getDico(diconame)
}

#' Title
#'
#' @param diconame 
#'
#' @return
#' @export
#'
#'  
getDico <- function(diconame) {
  ds <- getDicos()
  ds <-  subset(ds,ds$dico == diconame)
    
}

#' Title
#'
#' @param variablename 
#' @param actiontag 
#'
#' @return
#' @export
#'
#'  
getVarAction <- function(variablename,actiontag) {
  ds <- getDictionaryActions()
  ds <-  subset(ds,ds$variable == variablename & ds$action_group == actiontag )
}

#' Title
#'
#' @param variablename 
#' @param actiontag 
#'
#' @return
#' @export
#'
#'  
getVarActionParameters <- function(variablename,actiontag) {
  ds <- getVarAction(variablename,actiontag)
  ifelse(nrow(ds)>0, ds$parameters, NA)
}  

#' Title
#'
#' @param actiontag 
#'
#' @return
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


#' Title
#'
#' @return
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
