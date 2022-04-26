
#' fixEncoding
#'
#'  Add the encoding value to character vectors which contain special characters 
#'  (including accents)
#'  regular ascci values stay as "unknown"
#'
#' @param df A dataset to encode with new character set 
#' @param originalEncoding Either latin1 or UTF-8 which are the two encoding avalaible in R if 
#'
#' @return The encoded dataset 
#' @export
#'
#' 
fixEncoding <- function(df, originalEncoding = "UTF-8") {
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    # classename <-  as.character(class(df[, col]))
    if( inherits((df[, col]),"character") ){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(inherits((df[, col]), "factor") ){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(as.data.frame(df))
}


#' convEncoding 
#' 
#' convert character column in a dataset to a new encoding 
#' (old encoding should be given)
#'
#' @param df A dataset 
#' @param originalEncoding  The originale encoding value (accepted value can be obtained via iconvlist())
#' @param targetEncoding The target encoding value (For R should be latin1 or UTF-8)
#'
#' @return The newly encoded dataset 
#' @export
#'

convEncoding <- function(df, originalEncoding = "UTF-8",targetEncoding = NULL) {
  numCols <- ncol(df)
  df <- data.frame(df,stringsAsFactors=FALSE)
  if (is.null(targetEncoding)) {
    targetEncoding <- ifelse(l10n_info()$`UTF-8`==TRUE,"UTF-8","latin1")
  }  
  for (col in 1:numCols)
  {
    # if from contain more character than to, //TRANSLIT can be added to "to"in order to convert 
    # non existing character into the nearest  eg: to="ASCII//TRANSLIT"
    if(inherits(df[, col],"character")){
      df[, col] <- iconv(df[, col],sub="?",from=originalEncoding,to=targetEncoding)
    }
    
    if(inherits(df[, col] , "factor")){
      levels(df[, col])  <- iconv(levels(df[, col]),sub="?",from=originalEncoding,to=targetEncoding)
    }
  }
  return(as.data.frame(df))
}
