

#' sortBy
#'       Sort a dataframe according to a field optionnaly decreasing
#'        
#' @param data The dataframe to besorted 
#' @param field The field on which to sort (numeric or character). 
#' @param decreasing  Default is FALSE
#' @param na.last  Should NA be at the end ?
#'
#' @return The sorted dataframe
#' @export
#'
#' @examples
#' df <- data.frame(one=c(2,1,2,1),two=c(2,2,1,1))
#' df <- sortBy(df,"one")
#' df <- sortBy(df,1) 
sortBy <- function(data,field, decreasing=FALSE, na.last=TRUE) {
  data <- as.data.frame(data)
  sfield <- substitute(field)
  r <- try(class(field),TRUE)
  if ( inherits(r, "try-error")) {
     field <- with(data,sfield) 
  }  
  if (is.character(field) & length(field==1)) {
    vect <- data[,field]
  } else if (length(field)>1) {
    vect <-  field
  } 
  neworder <- order(vect ,na.last=na.last ,decreasing=decreasing )
  data[neworder,]
}

# nchar, lpad could be use if field is.vector  to concatenate field 
