

#' expandVar 
#' 
#' explode a variable which contain a list of values into Y/N variables according to the content of the "multi" variable
#' the list of expected values should be given with the names of the new variables
#'
#' @param df A dataset which contain a multi-variable
#' @param multivar The name of the multi-variable
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
expandVar <- function(df,multivar,valueslist) {

  dfname <- deparse(substitute(df))
  multivarname <- deparse(substitute(multivar))
  
  multivar <- df[[multivarname]]
  
  MyFun <-  function(x,valtosearch) {
    result  <-  grep(valtosearch,x )
    if (length(result) == 0) result <-0 else result <- 1
    if (result == 1) {
      return("Y")
    }
    else return("N")
  } 
  
  iMax <-  length(valueslist)  
  
  for (iNum in 1:iMax) {
    
    ValToSearch <-  names(valueslist)[iNum]
    ValToGrep <- paste0("\\<",ValToSearch,"\\>")
    NewCol <- vapply(multivar ,MyFun,valtosearch=ValToGrep,FUN.VALUE=" " )
    
    NameVar <- paste0(multivarname,"_",valueslist[[iNum]])
    df[[NameVar]] <- NewCol
    
  }
  return(df)
}  

