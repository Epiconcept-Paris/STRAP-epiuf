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
  # the dictionary dataset is assigned to the internal data after being updated  
  epidictionaryfiles_env$data <- updateDataset(dictionary,getNewDictionaryLine("dictionary"))
}

#' getDictionary
#'
#' @return The current dictionary
#' @export
#'
getDictionary <- function() {
  if (is.null(epidictionaryfiles_env$data)) createDictionary()
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
  # need more checks to verify that sheet exists with good name ! 
  if (file.exists(filename)) {
    epidictionaryfiles_env$datafilename <- filename
    # Create an if to add a warning message in case the sheet names don't match or do not exist
    sheet_names <- openxlsx::getSheetNames(filename)
    
    # Check if the modele sheets are found in the excel
    if ("dictionary" %in% sheet_names) {
      sheet1 <- readData(filename, sheet = "dictionary", verbose = F) # sheet dictionary
      if(all(is.na(sheet1))) warning("dictionary sheet is blank")
      if (!all(c("source_name", "generic_name", "type", "dico", "unknowns") %in%
               names(sheet1))) {
        warning("Sheet 'dictionary' not correct: ",
                paste0(c("source_name", "generic_name", "type", "dico", "unknowns"),collapse = ",") ,
                " cols needed.")
      }

      epidictionaryfiles_env$data <- sheet1
      epidictionaryfiles_env$data <- updateDataset(epidictionaryfiles_env$data,getNewDictionaryLine("dictionary"))
    } else {
      warning("Sheet dictionary not found")
      catret("\n")
    }
      
    if ("dicos" %in% sheet_names) {
      sheet2 <- readData(filename, sheet = "dicos", verbose = F)      # sheet dicos
      
      # test column names are correct
      if (!all(c("dico_name", "label", "code") %in% names(sheet2))) {
        warning("Sheet 'dicos' not correct: ",paste0(c("dico_name", "label", "code"), collapse = ",") ,
                " cols needed.")
      }
      # test if sheet is all blank 
      if(all(is.na(sheet2))) warning("dicos sheet is blank")
      
      epidictionaryfiles_env$dicos <- sheet2
      epidictionaryfiles_env$dicos <- updateDataset(epidictionaryfiles_env$dicos,getNewDictionaryLine("dicos"))

    } else {
      warning("Sheet dicos not found")
      catret("\n")
    }
    
    if ("actions" %in% sheet_names) {
      sheet3 <- readData(filename, sheet = "actions", verbose = F)    # sheet actions
      
      if (!all(c("variable", "action_group", "parameters") %in% names(sheet3))) {
        warning("Sheet 'actions' not correct: ",
                paste0(c("variable", "action_group", "parameters"), collapse = ",") ,
                " cols needed.")
      }
      if(all(is.na(sheet3))) warning("actions sheet is blank")
      
      epidictionaryfiles_env$actions <- sheet3
      epidictionaryfiles_env$actions <- updateDataset(epidictionaryfiles_env$actions,getNewDictionaryLine("actions"))

    } else {
      warning("Sheet actions not found")
      catret("\n")
    }

   } else {   # datadictionary doesn't exist we have to create it
      catret("")
      warning("Datadictionary ",filename," not found. Empty dictionary created\n")
      # we need to create the 3 data sheet
      createDictionary()
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

  if (is.null(filename)) {
    filename <- epidictionaryfiles_env$datafilename
    if (is.null(filename)) {
      stop('A filename should be given for saving dictionary')
    }
  }
  if(is.null(dictionary)) {
    ds <- getDictionary()
  } else ds <- dictionary
  
  if (nrow(ds)==0) {
    # replace by empty record ? 
    ds[1,] <- " "
  }
  #xlsx::write.xlsx(ds,file=filename,sheetName = "dictionary",row.names=FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"dictionary")
  openxlsx::writeData(wb,"dictionary",ds)
  
  ds <- getDicos()
  openxlsx::addWorksheet(wb,"dicos")
  openxlsx::writeData(wb,"dicos",ds)
  
  ds <- getDictionaryActions()
  openxlsx::addWorksheet(wb,"actions")
  openxlsx::writeData(wb,"actions",ds)
  
  openxlsx::saveWorkbook(wb,file=filename,overwrite=TRUE)
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
                              
  } else warning(mode," is not a dictionary sheet")
  return(OneDataLine)
}


#' getDicos
#'
#' @return The dataset containing all the dicos 
#' @export
#'

getDicos <- function() {
  ds <- epidictionaryfiles_env$dicos
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode="dicos")
  }
  if (nrow(ds)==0) ds[1,] <- NA
  epidictionaryfiles_env$dicos <-  ds
  return(ds)
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
  epidictionaryfiles_env$dicos <- updateDataset(dic,getNewDictionaryLine("dicos"))
}

#' getDictionaryActions
#'
#' @return A dataset of actions 
#' @export
#'
#'  
getDictionaryActions <- function() {
  ds <- epidictionaryfiles_env$actions
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode="actions")
  }
  if (nrow(ds)==0){ ds[1,] <- NA}
  epidictionaryfiles_env$actions <-  ds
  return(ds)
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
#' Retrieve the value of one parameters (column) in the dictionary, searching for the generic_name
#' usual column values to retrieve are for type, dico, unknowns
#' Give an error if column name is incorrect
#' Return NA if searched varname is not found  
#'
#' @param varname The varname for which we will retrieve content of one column from dictionary
#' @param valuename Name of the coulumn to retrieve from dictionary
#'
#' @return A single value
#' @export
#'
#'  
getDictionaryValue <- function(varname, valuename=c("type","dico","unknowns")) {
  ds <- getDictionary()
  value <-  NA
  if (nrow(ds)>0) {
     paramok <- (valuename%in%names(ds))
     if (paramok) {
       value <- subset(ds,ds$generic_name == varname)[,valuename]
       if (length(value)==0) value <- NA
     } else warning(valuename," is not allowed as a dico column")    
  }  
  return(value)
}

#' Retrieve values from a dictionary based on variable name and specified columns
#'
#' This function searches a dictionary for a given variable name and returns the corresponding
#' values from specified dictionary columns. It issues a warning if the search column or the
#' value column is not present in the dictionary.
#'
#' This function is advanced and you should usually use getDicoOfVar or getVarAction 
#'
#' @param varname The variable name to search for in the dictionary.
#' @param searchcolumn A character vector specifying which column(s) to search in the dictionary.
#'   Defaults to c("source_name", "generic_name").
#' @param value A character vector specifying which column(s) to return values from.
#'   Defaults to c("source_name", "generic_name", "dico", "type", "unknowns").
#'
#' @return Returns the subset of the dictionary that matches the search criteria or `NA` if no
#'   matches are found or if the search/value columns are not in the dictionary.
#' @export
#' 
#' @examples
#' # Assuming 'getDictionary' is a function that returns a data frame and 'varname' is a known variable
#' getAnyDictionaryValue(varname = "exampleVar")
#' \dontrun{getAnyDictionaryValue("varname",searchcolumn="source_name",value="dico")}
getAnyDictionaryValue <- function(varname,
                                  searchcolumn = c("source_name","generic_name"), 
                                  value=c("source_name","generic_name","dico","type","unknowns","description","comments")) {
  ds <- getDictionary()
  result <-  NA
  # if dictionary is not empty
  if (nrow(ds)>0) {
    
    if( ! searchcolumn%in%names(ds)){
      warning(searchcolumn," is not allowed as a dico column")   
      return(result) 
    }
    
    if(! value%in%names(ds)) {
      warning(value," is not allowed as a dico column")   
    return(result) 
    } 
    # looks for varname in searchcolumn and return content of value
    result <- subset(ds,ds[,searchcolumn] == varname)[,value]
    if (length(result)==0) result <- NA
  }  
  return(result)
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
   diconame <- getDictionaryValue(varname,"dico")
   if (!is.na(diconame)){
     dic <- getDico(diconame)
   } else cat("No dico associated with",varname)
   return(dic)
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
  if (length(ds)==0) {
    ds <- NA
    ## PR_CLZ : add line before and after
    catret("")
    red("Dico",diconame,"not found")
    catret("\n")
    ## END_PR_CLZ 
  }  
  return(ds) 
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
    if (length(ds)==0) {
      ds <- NA
      if (is.na(getActionGroup(actiontag))){
        red(actiontag,"is not found as a valid actiontag ")
      }
    }  
    return(ds)
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
  # ds <- ifelse(nrow(ds)>0, ds$parameters, NA)
  ds <- ds$parameters
  return(ds)
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
  ds <- if(nrow(ds)>0){
    return(ds)}else{
      return(NA) 
    }
}



#' applyDictionary
#'
#' @param dictionary A dictionary (passed as dataframe)
#' @param data  A dataset to transform to generic
#' @param verbose Should we have feedback 
#' @param keepextra if TRUE, extra variables existing in data but not in generic dictionary are keep
#'  in the returned dataset. Then this generic dataset is no longer generic because it may contain 
#'  non generic variables      
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
  dupName <- anyDuplicated(NewNames)
  if (! (dupName==0)) warning(NewNames[[dupName]]," is duplicated in New name list")
  
  # we make a character vector of sources names from dictionary
  OldNames <- getColValues(dictionary,dicSourceName)
  dupName <- anyDuplicated(OldNames)
  if (! (dupName==0)) warning(OldNames[[dupName]]," is duplicated in Old name list")
  
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
      data <- data[, -which( (names(data) %in% VarExtra) & !(names(data)%in%NewNames)  )]
    } else {
      epiuf::bold("Extra vars in imported keept in generic  : ",length(VarExtra))
      catret()
      catret(sort(VarExtra),sep="  \n")
      data <- data  
    }
  } else { data <- data } 
  
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
  if (length(todrop) > 0 ) {
    data <- data[, -which( (names(data) %in% todrop) )]
  }  
  
  MatchNames <- MatchNames[! is.na(MatchNames[dicGenericName]),]
  MatchNames <- MatchNames[! (MatchNames[dicGenericName] == MatchNames[dicSourceName] ),]
  nbToRename <- nrow(MatchNames)
  if (verbose==TRUE) {
    bold("Imported vars renamed with a generic name : ", nbToRename)
    catret()
    listMatchNames <- list()
    
    if (nbToRename>0) {
      for (i in 1:nbToRename ){
        listMatchNames[i] <- paste(MatchNames[i,dicGenericName],"<=",MatchNames[i,dicSourceName])
      }
      catret(unlist(listMatchNames),sep="\n")
      if (nbToRename > 20) {
        catret("...")
      }  
    }  
  }
  # rename the existing
  if (nbToRename > 0 ) {
    for (i in 1:nrow(MatchNames)) {
      thenewname <- MatchNames[i,dicGenericName]
      theoldname <- MatchNames[i,dicSourceName]
      names(data)[names(data) == theoldname] <- thenewname 
    }
  }
  #now check for missing in final
  CurNames <- unlist(names(data))
  VarMiss <- setdiff(NewNames,CurNames)
  nbToAdd <- length(VarMiss)
  if (nbToAdd>0){ 
    for (i in 1:nbToAdd ) {
      typevar <-  dictionary[dictionary[[dicGenericName]]==VarMiss[i],"type"]
      typevar <- typevar[! is.na(typevar)]
      valuevar <- switch (typevar,
                          "numeric" = as.numeric(NA), ## EDIT all to be NA and not a value
                          "character" = as.character(NA),
                          "date" = as.Date(NA),
                          NA
      )
      data[,VarMiss[i]] <- valuevar
    } 
  }  
  if (verbose==TRUE) {
    bold("Generic vars created (as empty) : ", length(VarMiss))
    catret()
    catret(sort(VarMiss), sep=", ")
  }
  
  data
}


#' Create a new dictionary environment with default entries
#'
#' This function initializes a new dictionary environment for epidemiological data management.
#' It sets up the environment with default dictionary entries. The environment contains separate
#' entries for the dictionary data, dicos, and actions, each initialized with a line from a
#' corresponding 'getNewDictionaryLine' function.
#'
#' @param filename Optional; a character string providing the name of the file to be associated with
#'   the dictionary. Defaults to an empty string.
#'
#' @return Does not return a value; called for side effects of setting up the dictionary environment.
#'
#' @examples
#' # Create a new dictionary with default settings
#' createDictionary()
#' 
#' @export
createDictionary <- function(filename="") {
  # base 
  epidictionaryfiles_env$datafilename <- filename
  epidictionaryfiles_env$data <- getNewDictionaryLine("dictionary")
  epidictionaryfiles_env$dicos <- getNewDictionaryLine("dicos")
  epidictionaryfiles_env$actions <- getNewDictionaryLine("actions")
} 



# END of SCRIPT  --------------------------------------------------------
