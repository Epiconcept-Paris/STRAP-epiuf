
#' fixEncoding
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
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(class(df[, col]) == "factor"){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(as_data_frame(df))
}
