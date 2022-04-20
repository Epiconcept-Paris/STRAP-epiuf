

#' Title
#'
#' @param varname 
#' @param inputname 
#' @param valuename 
#'
#' @return
#' @export
#'
#' @examples
getAnyDictionaryValue <- function(varname,inputname = c("source_name","generic_name","dico","type","unknowns"), valuename=c("source_name","generic_name","dico","type","unknowns")) {
  ds <- getDictionary()
  value <-  NA
  if (nrow(ds)>0) {
    paramok <- (valuename%in%names(ds))
    paramok2 <- (inputname%in%names(ds))
    if (paramok & paramok2) {
      value <- subset(ds,ds[,inputname] == varname)[,valuename]
      if (length(value)==0) value <- NA
    } else warning(inputname, "or", valuename," is not allowed as a dico column")    
  }  
  return(value)
}
