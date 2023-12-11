

#' expandVar 
#' 
#' explode a variable which contain a list of values into Y/N variables according to the content of the "multi" variable
#' the list of expected values should be given with the names of the new variables
#'
#' @param data A dataset which contain a multi-variable
#' @param varname The name of the multi-variable
#' @param valueslist List of expected values with names of the variables to be created
#'
#' @return A df with the new variables
#' @export
#'
#' @examples
#' 
#' data <- data.frame(Id = 1:4 , 
#'                     Vaccs = c("pfizer,moderna"," ", "pfizer", "moderna"))
#' brand <- list("pfizer"="pfizer",
#'              "moderna"="moderna"
#'               )
#' data <-  expandVar(data,Vaccs,brand)
#' 
#' 

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
