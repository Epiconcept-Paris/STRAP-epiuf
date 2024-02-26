#
# Project Name : STRAP
# Script Name  : setTypeAll
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of setTypeAll function
# Date created : 27/11/23
# Author       : JHD
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------




# Changes Log --------------------------------------------------------------
#  
# 
# 

# START of SCRIPT  --------------------------------------------------------

#' setTypeAll
# Function to set data types for all variables in a dataset based on a given dictionary.
# If no dictionary is provided, it retrieves a default dictionary from the global environment.
#' 
#' @param data The dataset
#' @param dictionary The epiuf dictionary
#'
#' @return data frame
#' @export
#'
#' @examples
#'  \dontrun{
#'    setTypeAll(data)
#'    }
#' 
setTypeAll <- function(data, dictionary=NULL){
  
  # Use the custom dictionary if provided, otherwise retrieve the default dictionary
  
  if(is.null(dictionary)){       
    ds <- getDictionary()
  }else{
    ds <- dictionary
  }
  
  # Find variables in dataset that are also present in the dictionary
  relevantVariables <- intersect(colnames(data), ds$generic_name) 
  
  # Loop through each relevant variable to set its type
  for (variable in relevantVariables){
    typeInfo <- getDictionaryValue(variable, "type", dictionary = ds) # retrieve list of type if present. (NA if none given)
    
    # If type information is available, update the variable type in the dataset
    if (!is.na(typeInfo)){
      data[,variable] <- setType(data, variable, typeInfo)
    }
  }
  # Return the modified dataset
  return(data)
}



# END of SCRIPT  --------------------------------------------------------

