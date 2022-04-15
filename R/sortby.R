

#' sortBy
#'       Sort a dataframe according to a field optionnaly decreasing
#'        
#' @param data The dataframe to besorted 
#' @param field The field on which to sort (numeric or character)
#' @param decreasing  Default is FALSE
#'
#' @return The sorted dataframe
#' @export
#'
#' @examples
#' df <- data.frame(one=c(3,1,5,2))
#' df <- sort(df,"one")
#' df <- sort(df,1) 
sortBy <- function(data, field, decreasing=FALSE, na.last=TRUE) {
  data <- as.data.frame(data)
  vect <- data[,field]
  neworder <- order(vect ,na.last=na.last ,decreasing=decreasing )
  data[neworder,]
}


# sort(data,by="colname")