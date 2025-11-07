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


#' setDictionary
#'
#' Set a dictionary using a data.frame (this function would be rarely used except for tests).
#'
#'
#' @param dictionary A dictionary epiuf structure (data.frame)
#'
#' @return No return value
#' @seealso [getDictionary()], [openDictionary()], [applyDictionary()]
#' @examples
#' 
#' # Create an example data frame to set as a dictionary 
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"))
#' 
#' # Set the dictionary 
#' setDictionary(dictionary = dic)
#' 
#' 
#' @export
#'

setDictionary  <- function(dictionary) {
  # the dictionary dataset is assigned to the internal data after being updated  
  epidictionaryfiles_env$data <- updateDataset(dictionary, getNewDictionaryLine("dictionary"))
}


#' getDictionary
#'
#' This function loads the most recent dictionary set into the environment. 
#'
#' @return The current dictionary
#' @seealso [setDictionary()]
#' @export
#' @examples
#' # Create an example data frame to set as a dictionary 
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"))
#' 
#' # Set the current dictionary 
#'  setDictionary(dictionary = dic)
#'    
#' # Load dictionary into the environment 
#' retrieved <- getDictionary()
#'
getDictionary <- function() {
  if (is.null(epidictionaryfiles_env$data)) createDictionary()
  return(epidictionaryfiles_env$data)
}


#' openDictionary
#'
#' Imports a dictionary into the environment. <br>
#' 
#' Warning: The dictionary needs to be in 'epiuf' structure 
#' (see example \code{externalFile("genericdictionary.xlsx")}) with three sheets:
#' * \code{'dictionary'} including a table with the following columns:
#'    * \code{'source_name'}, name of the variable in the source file
#'    * \code{'generic_name'}, generic name replacement of the variable
#'    * \code{'type'}, variable type
#'    * \code{'unknowns'}, values to be considered as unknowns (e.,g., UNK, 9, 999)
#'    * \code{'dico'}, name of the corresponding dictionary
#' * \code{'dicos'} including a table with the following columns:
#'    * \code{'dico_name'}, name of the ref dico (e.g., YESNO)
#'    * \code{'label'}, label of each dico value (e.g., Yes)
#'    * \code{'code'}, code of each dico value (e.g., 1)
#' * \code{'actions'} including a table with the following columns:
#'    * \code{'variable'}
#'    * \code{'action_group'}, action to run on the above-mentioned variable
#'    * \code{'parameters'}, parameters to use in above-mentioned action/function
#'    
#' @param filename Character string, path and file name to the file (xls) 
#' containing the dictionary to open. The dictionary will be loaded.
#'
#' @return No return value
#' @seealso [setDictionary()], [getDictionary()], [applyDictionary()]
#' 
#' @examples
#' file1 <- externalFile("genericdictionary.xlsx")
#' openDictionary(file1)
#' 
#' \dontrun{
#' openDictionary(pathToFile("REFERENCE", "RefDictionnary.xlsx")))
#' }
#' 
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
      sheet1 <- readData(filename, sheet = "dictionary", verbose = FALSE) # sheet dictionary
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
#' Function to save the current dictionary \code{'dictionary'} as an Excel 
#' spreadsheet in a given folder \code{'filename'}.
#'
#' @param filename The file name and path where to save the dictionary (character). 
#' Any existing file will be overwritten. 
#' @param dictionary (Optional) Dictionary. By default the current dictionary will be saved.
#'
#' @return No return value
#' @seealso [getDictionary()]
#' @export
#' 
#' @examples 
#' \dontrun{
#' saveDictionary(filename = paste0(getwd(), "/test.xlsx"))
#' }
#'

saveDictionary <- function(filename = NULL,
                           dictionary = NULL) {

  if (is.null(filename)) {
    filename <- epidictionaryfiles_env$datafilename
    if (is.null(filename)) {
      stop('A filename should be given for saving dictionary')
    }
  }
  if(is.null(dictionary)) {
    ds <- getDictionary()
  } else ds <- dictionary
  
  if (nrow(ds) == 0) {
    # replace by empty record ? 
    ds[1,] <- " "
  }
  #xlsx::write.xlsx(ds,file=filename,sheetName = "dictionary",row.names=FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "dictionary")
  openxlsx::writeData(wb, "dictionary", ds)
  
  ds <- getDicos()
  openxlsx::addWorksheet(wb, "dicos")
  openxlsx::writeData(wb, "dicos", ds)
  
  ds <- getDictionaryActions()
  openxlsx::addWorksheet(wb, "actions")
  openxlsx::writeData(wb, "actions", ds)
  
  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
}

# we need to add the current status ! 
#' getNewDictionaryLine
#'
#' @param mode Type of line to return (character). Could be dictionary, dicos, or actions 
#' (one of the three tabs of the Excel dictionary file, see \code{openDictionary()})
#'
#' @return An empty record in a data frame of type \code{mode}
#' @export
#' @seealso [openDictionary()]
#' 
#' @examples
#' getNewDictionaryLine(mode = "dictionary")
#' getNewDictionaryLine(mode = "dicos")
#' getNewDictionaryLine(mode = "actions")
#' 
#'

getNewDictionaryLine  <- function(mode = "dictionary") {

  # LMC 2025-11-06: I believe we should initiate the result value with
  # OneDataLine <- NA
  
  if (mode == "dictionary") {  
  OneDataLine <- data.frame(source_name = as.character(),               # name in the source
                            generic_name = as.character(),              # generic name replacement 
                            type = as.character(),                      # type of the variable
                            dico = as.character(),
                            unknowns = as.character(),
                            description = as.character(),
                            comments = as.character(),
                            stringsAsFactors = FALSE
                            )
  } else if (mode == "dicos") {
    OneDataLine <- data.frame(dico_name = as.character(),               
                              label = as.character(),               
                              code = as.character(),
                              stringsAsFactors = FALSE
                              )
                              
  } else if (mode == "actions") {
    OneDataLine <- data.frame(variable = as.character(),               
                              action_group = as.character(),               
                              parameters = as.character(),
                              stringsAsFactors = FALSE
                              )
                              
  } else warning(mode, " is not a dictionary sheet")
  return(OneDataLine)
}


#' getDicos
#'
#' This function returns a dataset containing all stored dicos, including 
#' the list of codes and labels, as returned by \code{getNewDictionaryLine("dicos")}.
#' 
#' @return The dataset containing all the dicos in 3 columns: dico_name, label, code.
#' @export
#' @seealso [setDicos()]
#' 
#' @examples
#' getDicos() 
#' 
#'

getDicos <- function() {
  ds <- epidictionaryfiles_env$dicos
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode = "dicos")
  }
  if (nrow(ds)==0) ds[1,] <- NA
  epidictionaryfiles_env$dicos <-  ds
  return(ds)
}


#' setDicos
#' 
#' Set the dataset \code{dic} in the format of epiuf structure "dicos" 
#' as returned by \code{getNewDictionaryLine("dicos")} 
#' (i.e., columns: 'dico_name', 'label', 'code')
#' and store it in environment.
#' 
#' @param dic A dataset of dicos (with epiuf structure as returned by \code{getNewDictionaryLine("dicos")})
#'
#' @return No return value
#' @export
#' @seealso [getDicos()], [getNewDictionaryLine()]
#' 
#' @examples
#' di <- data.frame(dico_name = rep("yesno", 2),
#'                  label = c("yes", "no"),
#'                  code = c("1", "0"))
#' setDicos(di)  
#' getDicos()
#'  
setDicos <- function(dic) {
  epidictionaryfiles_env$dicos <- updateDataset(dic, getNewDictionaryLine("dicos"))
}


#' getDictionaryActions
#' 
#' Return the dictionary actions dataset stored in the environment 
#' as returned by \code{getNewDictionaryLine("actions")}..
#'
#' @return A dataset of actions 
#' @export
#' @seealso [setDictionaryActions()], [getNewDictionaryLine()]
#' 
#' @examples
#' getDictionaryActions()
#'
#'  
getDictionaryActions <- function() {
  ds <- epidictionaryfiles_env$actions
  if (is.null(ds) ) {
    # replace by empty record ? 
    ds <- getNewDictionaryLine(mode = "actions")
  }
  if (nrow(ds) == 0){ ds[1,] <- NA}
  epidictionaryfiles_env$actions <-  ds
  return(ds)
}


#' setDictionaryActions
#' 
#' Set the data frame \code{actions} as a dictionary action in the environment
#' as returned by \code{getNewDictionaryLine("actions")} 
#' (i.e., columns: 'variable', 'action_group', 'parameters').
#'
#' @param actions A dataset (data frame) of actions 
#'
#' @return No return value
#' @export
#' @seealso [getDictionaryActions()], [getNewDictionaryLine()]
#' 
#' @examples
#' # Example of empty dictionary action
#' getNewDictionaryLine(mode = "actions")
#' 
#' # Set it as dictionary action in environment
#' setDictionaryActions(getNewDictionaryLine(mode = "actions"))
#' 
#'
#'  
setDictionaryActions <- function(actions) {
  epidictionaryfiles_env$actions <- actions
}


#' getDictionaryValue
#' 
#' Retrieve the value of one parameter's (column) in the dictionary, searching for the 'generic_name'.
#' Usual column values to retrieve are: type, dico and unknowns.
#' An error will occur if the column name is incorrect.
#' Return NA if searched varname is not found. 
#'
#' @param varname The varname for which we will retrieve content of one column from the dictionary (character).
#' @param valuename Name of the column to retrieve from dictionary (character).
#'
#' @return A single value
#' @seealso [getNewDictionaryLine()]
#' 
#' @examples
#' 
#' # Create example dataset 
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"))
#' 
#' # Set the dictionary 
#' setDictionary(dictionary = dic)
#' 
#' # Use the function
#' getDictionaryValue("agegroup3","dico")
#' 
#' @export
#'
#'  
getDictionaryValue <- function(varname, 
                               valuename = c("type", "dico", "unknowns")) {
  ds <- getDictionary()
  value <-  NA
  if (nrow(ds)>0) {
     paramok <- (valuename %in% names(ds))
     if (paramok) {
       value <- subset(ds, ds$generic_name == varname)[,valuename]
       if (length(value) == 0) value <- NA
     } else warning(valuename, " is not allowed as a dico column")    
  }  
  return(value)
}


#' getAnyDictionaryValue 
#' 
#' Retrieve values from a dictionary based on variable name and specified columns.
#' This function searches a dictionary for a given variable name and returns the corresponding
#' values from specified dictionary columns. It issues a warning if the search column or the
#' value column is not present in the dictionary.
#'
#' This function is advanced and you should usually use \code{getDicoOfVar} or \code{getVarAction} 
#'
#' @param varname The variable name to search for in the dictionary (character).
#' @param searchcolumn A character vector specifying which column(s) to search in the dictionary.
#'   Defaults to \code{c("source_name", "generic_name")}.
#' @param value A character vector specifying which column(s) to return values from.
#'   Defaults to \code{c("source_name", "generic_name", "dico", "type", "unknowns")}.
#'
#' @return Returns the subset of the dictionary that matches the search criteria or `NA` if no
#'   matches are found or if the search/value columns are not in the dictionary.
#' @export
#' @seealso [getDicoOfVar()], [getVarAction()]
#' 
#' @examples
#' # Assuming 'getDictionary' is a function that returns a data frame and 'varname' is a known variable
#' getAnyDictionaryValue(varname = "exampleVar")
#' 
#' \dontrun{
#' getAnyDictionaryValue("varname",
#'                       searchcolumn = "source_name",
#'                       value = "dico")
#' }
#' 
#' # Set an example dictionary
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"))
#' 
#' # Set the current dictionary 
#'  setDictionary(dictionary = dic)
#'  
#' # Search for the value 'agegp3' in the column 'dico' and returning 
#' # the corresponding value in column 'generic_name'
#' getAnyDictionaryValue(varname = "agegp3",
#'                       searchcolumn = "dico",
#'                       value = "generic_name")
#'            
#' 

getAnyDictionaryValue <- function(varname,
                                  searchcolumn = c("source_name", "generic_name"), 
                                  value = c("source_name", "generic_name", 
                                            "dico", "type", "unknowns",
                                            "description", "comments")) {
  ds <- getDictionary()
  result <-  NA
  # if dictionary is not empty
  if (nrow(ds)>0) {
    
    if( ! searchcolumn %in% names(ds)){ #LMC 2025-11-07: I think it should be: if(sum(searchcolumn %in% names(ds)) == 0)
      warning(searchcolumn," is not allowed as a dico column")   
      return(result) 
    }
    
    if(! value %in% names(ds)) { #LMC 2025-11-07: I think it should be: if(sum(value %in% names(ds)) == 0)
      warning(value," is not allowed as a dico column")   
    return(result) 
    } 
    # looks for varname in searchcolumn and return content of value
    result <- subset(ds, ds[, searchcolumn] == varname)[, value]
    if (length(result)==0) result <- NA
  }  
  return(result)
}


#' getDicoOfVar
#' 
#' Return the dico associated with the variable `varname` (as defined in `generic_name`) 
#' in the format as returned by \code{getNewDictionaryLine("dicos")}
#'
#' @param varname The variable for which we want to retrieve the name of the associated dico (character)
#'
#' @return The name of the dico associated with the variable
#' @export
#' @seealso [getNewDictionaryLine()], [getAnyDictionaryValue()]
#'
#' @examples
#' # Create dummy dictionary
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"),
#'                   unknowns = NA,
#'                   description = NA,
#'                   comments = NA)
#' 
#' # Set the current dictionary 
#' setDictionary(dictionary = dic)
#' 
#' # Create corresponding dummy dicos  
#' dicos <- data.frame(dico_name = c("yesno", "yesno", "agegp3", "agegp3", "agegp3"),
#'                     label = c("no", "yes", "0-14 years", "15-64 years", "65+ years" ),
#'                     code = c("0", "1", "0", "1", "2"))
#' # Set the corresponding dicos
#' setDicos(dicos)
#' 
#' # Retrieve the corresponding dictionary of variable "agegroup3" (see 'generic_name')
#' getDicoOfVar("agegroup3")
#' 
#'  
getDicoOfVar <- function(varname) {
   diconame <- getDictionaryValue(varname, "dico")
   if (!is.na(diconame)){
     dic <- getDico(diconame)
   } else cat("No dico associated with",varname)
   return(dic)
}


#' getDico
#'
#' This function returns a data set containing one dico in the format 
#' as returned by \code{getNewDictionaryLine("dicos")} (i.e., `dico_name`, `label`, `code`).
#'
#' @param diconame The name of one dico from the dicos structure (character)
#'
#' @return A data set containing one dico (list of code/labels)
#' @export
#' @seealso [getDicos()], [getNewDictionaryLine()]
#' 
#' @examples
#' # Create corresponding dummy dicos  
#' dicos <- data.frame(dico_name = c("yesno", "yesno", "agegp3", "agegp3", "agegp3"),
#'                     label = c("no", "yes", "0-14 years", "15-64 years", "65+ years" ),
#'                     code = c("0", "1", "0", "1", "2"))
#' # Set the corresponding dicos
#' setDicos(dicos)
#' 
#' # Retrieving all dicos
#' getDicos()
#' 
#' # REtriving the dico of interest
#' getDico("yesno")
#' 
#'  
getDico <- function(diconame) {
  ds <- getDicos()
  ds <- subset(ds, ds$dico == diconame)
  if (length(ds) == 0) { #LMC 2025-11-07: I think that length(ds) should be replaced by nrow(ds)
    ds <- NA
    ## PR_CLZ : add line before and after
    catret("")
    red("Dico",diconame,"not found") #LMC 2025-11-07: To replace with an error and avoid returning NA
    catret("\n")
    ## END_PR_CLZ 
  }  
  return(ds) 
}


#' getVarAction
#'
#' Retrieve a certain action from the dictionary action tab (see structure of 
#' \code{getNewDictionaryLine("actions")}) for a given variable (see column `variable`).
#'
#'
#' @param variablename The variable (character) for which we want to retrieve 
#' the associated action (see column `variable` in `getDictionaryActions()`).
#' @param actiontag The name of the action group to retrieve (character)
#' (see column `action_group` in `getDictionaryActions()`).
#'
#' @return A dataset of var actions records for the variable. 
#' @export
#' @seealso [getDictionaryActions()], [getNewDictionaryLine()]
#'
#'  
getVarAction <- function(variablename, actiontag) {
  ds <- getDictionaryActions()
#GDE check to be added for wrong action name
    ds <-  subset(ds, ds$variable == variablename & ds$action_group == actiontag )
    if (length(ds)==0) { #LMC 2025-11-07: need to replace length() with nrow()
      ds <- NA
      if (is.na(getActionGroup(actiontag))){
        red(actiontag,"is not found as a valid actiontag ")
      }
    }  
    return(ds)
}


#' getVarActionParameters
#'
#' Retrieve the associated action parameter (i.e., value in `parameters` column) 
#' from a given variable (i.e., defined in the column `variable` in `getDictionaryActions()`)
#' and given action (i.e., value in `action_group` column). 
#'
#' @param variablename The variable (character) for which we want to retrieve 
#' the associated action parameters (see columns `variable` in `getDictionaryActions()`).
#' @param actiontag The name of the action group (character)
#' (see column `action_group` in `getDictionaryActions()`). 
#'
#' @return The parameters associated to the variable/action (see column `parameters` in `getDictionaryActions()`)
#' @export
#' @seealso [getDictionaryActions()], [getNewDictionaryLine()], [getVarAction()]
#'
#'  
getVarActionParameters <- function(variablename, actiontag) {
  ds <- getVarAction(variablename, actiontag)
  # ds <- ifelse(nrow(ds)>0, ds$parameters, NA)
  ds <- ds$parameters
  return(ds)
}  


#' getActionGroup
#'
#' Return the dictionary action (see \code{getDictionaryActions()}) filtered 
#' for a given `action_group`.
#'
#' @param actiontag Name of the action group to retrieve (character)
#'
#' @return Dataset of dictionary actions containing all the variables with actions of type action group
#' @export
#' @seealso [getDictionaryActions()], [getVarAction()]
#'  
getActionGroup <- function(actiontag) {
  ds <- getDictionaryActions()
  ds <-  subset(ds, ds$action_group == actiontag )
  ds <- if(nrow(ds) > 0){ #LMC 2025-11-07: ds to be removed
    return(ds)
  }else{ #LMC 2025-11-07: We should have the same behaviour as in getVarAction()
    return(NA) 
  }
}



#' applyDictionary
#' 
#' Updates dataset `data` and transform it according to the dictionary generic structure.  
#'
#' @param dictionary A dictionary (data.frame)
#' @param data  A dataset to transform to generic structure of the dictionary (data.frame)
#' @param verbose Feedback regarding matching of the two
#' @param keepextra Logical. If TRUE, extra variables existing in data 
#' but not in generic dictionary are kept in the returned dataset. 
#' Then this generic dataset is no longer generic because it may contain 
#' non generic variables.      
#'
#' @return A data set 
#' @export
#' @examples 
#'  
#' # Create dummy dictionary
#' dic <- data.frame(generic_name = c("id", "idcountry", "age", "agegroup3", "agroup10"),
#'                   source_name = c("ID", "CountryID", "Age", "Agegp3", "Agep10"),
#'                   type = c("character", "character", "numeric", "factor", "factor"),
#'                   dico = c("","","","agegp3","agegp10"),
#'                   unknowns = NA,
#'                   description = NA,
#'                   comments = NA)
#' 
#' # Set the current dictionary 
#' setDictionary(dictionary = dic)
#' 
#' # Create corresponding dummy dicos  
#' dicos <- data.frame(dico_name = c("yesno", "yesno", "agegp3", "agegp3", "agegp3"),
#'                     label = c("no", "yes", "0-14 years", "15-64 years", "65+ years" ),
#'                     code = c("0", "1", "0", "1", "2"))
#' 
#' # Set the corresponding dicos
#' setDicos(dicos)
#' 
#' # Dummy source data
#' source <- data.frame(ID = c(1,2,3),
#'                      CountryID = c(2,3,4),
#'                      Age = c(4,5,6))
#' 
#' # Transforming the source data into generic
#' new <- applyDictionary(dictionary = dic,
#'                        data = source)
#'                        
#' # Example base on epiuf::DummyData          
#' new <- applyDictionary(dictionary = dic, 
#'                        data = epiuf::DummyData)
#' new <- applyDictionary(dictionary = dic, 
#'                        data = epiuf::DummyData,
#'                        keepextra = TRUE)
#'                
#'

applyDictionary <- function( dictionary = NULL, 
                             data, 
                             verbose = TRUE, 
                             keepextra = FALSE) {
  
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
#' This function initialises a new dictionary environment for epidemiological data management.
#' It sets up the environment with default dictionary entries. The environment contains separate
#' entries for the dictionary data, dicos, and actions, each initialized with a line from a
#' corresponding 'getNewDictionaryLine' function.
#'
#' @param filename Optional; a character string providing the name of the file to be associated with
#'   the dictionary. Defaults to an empty string.
#'
#' @return Does not return a value; called for side effects of setting up the dictionary environment.
#'@seealso [getNewDictionaryLine()], [openDictionary()]
#'
#' @examples
#' # Create a new dictionary with default settings
#' createDictionary()
#' 
#' @export
createDictionary <- function(filename = "") {
  # base 
  epidictionaryfiles_env$datafilename <- filename
  epidictionaryfiles_env$data <- getNewDictionaryLine("dictionary")
  epidictionaryfiles_env$dicos <- getNewDictionaryLine("dicos")
  epidictionaryfiles_env$actions <- getNewDictionaryLine("actions")
} 



# END of SCRIPT  --------------------------------------------------------
