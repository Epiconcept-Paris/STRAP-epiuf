

#' expandVar 
#' 
#' To expand a variable containing more than one value into new individual variables with Y/N values.
#' The list of expected values must be passed as parameters together with the name of the new variable to be created.
#' 
#'
#' @param data A dataset which contains the variable with more than one value (data.frame). 
#' @param varname The name of the variable. 
#' @param valueslist List of expected values with names of the variables to be created.
#'
#' @return A dataset with the new columns added.
#' @export
#'
#' @examples
#' 
#' # Create an example data frame 
#' data <- data.frame(Id = 1:4 , 
#'                     Vaccs = c("pfizer,moderna"," ", "pfizer", "moderna"))
#'
#' # Specify the name of the new column and what value the new column represents 
#' brand <- list("pfizer"="pfizer",
#'              "moderna"="moderna"
#'               )
#' # Use the function
#' data <-  expandVar(data = data, varname = Vaccs, valueslist = brand)
#' 
#' # View the changed dataset 
#' print(data)

expandVar <- function(data,varname,valueslist) {
  
  s_op <- deparse(substitute(varname))
  # if varname is a variable which contain char, we use content of varname
  tryCatch(
    if (is.character(varname)) {
      s_op <- varname
    }
    , error = function(c) { }
  )
  varname <- s_op
  
  var <- data[[varname]]
  
  MyFun <-  function(x,valtosearch) {
    result  <-  grep(valtosearch,x )
    if (length(result) == 0) result <-0 else result <- 1
    if (result == 1) {
      return("1")
    }
    else return("0")
  } 
  
  iMax <-  length(valueslist)  
  
  for (iNum in 1:iMax) {
    
    ValToSearch <-  names(valueslist)[iNum]
    ValToGrep <- paste0("\\<",ValToSearch,"\\>")
    NewCol <- vapply(var ,MyFun,valtosearch=ValToGrep,FUN.VALUE=" " )
    
    NameVar <- paste0(varname,"_",valueslist[[iNum]])
    data[[NameVar]] <- as.numeric(NewCol)
    
  }
  return(data)
} 
