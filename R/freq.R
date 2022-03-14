# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#' #'
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#' @export
#' @param x As numbers, factors or text.
#' @param missing If false then missing values are not included in the table
#'   A summary output of number of missing values is added at the end
#' @param quietly No output, only return value
#' @return An array containing  values of \code{...}   \code{}
#'
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'
freq <- function(x,missing=FALSE,quietly = FALSE) {
  var.name <- deparse(substitute(x))
  cur.var <- x
  if (! is.null(cur.var)) {
    count <- table(cur.var, useNA=ifelse(missing,"ifany","no") )
    tot <- length(cur.var)
    prop <- round(prop.table(count)*100, digits = 2)
    cum <- cumsum(prop)
    result <- cbind(count,
                    prop,
                    cum)
    mis  <- sum(is.na(cur.var))
    colnames(result) <- c("Freq", "%" , "cum%")
    result <- rbind(result,Total = colSums(result))
    cdeci <- c(FALSE,TRUE,TRUE)
    deci <- 1
    result[nrow(result),ncol(result)] <- 100
    title <- paste("Frequency distribution of ",var.name)
    names(dimnames(result))  <- c(var.name,title)
    if (! quietly) {
      # outputtable(result,deci,totcol=FALSE,title=title,coldeci=cdeci )
    }
    # missing should be added to result
    if (! missing) {
      cat("Missing (NA):",mis," (",round(mis/tot*100, digits = 2),"%)\n")
    }
    # construct of returned list
    r <- list()
    r$table <- result
    r$total <- tot
    r$missing <- mis
    invisible(r)
    result
  }
}




tab <- function( ..., miss="ifany", data=df) {
  # first we catch the ... parameters
  listvar <- as.list(match.call())
  # we remove the first one which is the function name
  listvar[1] <- NULL
  # and we remove all named parameters from the list
  namedarg <- pmatch(c("data", "miss"),names(listvar), nomatch = 0)
  if (length(namedarg) > 0 ) {listvar[namedarg] <- NULL }
  
  # We verify that parameters are language and if it is "test" then we parse as language
  if (length(listvar)>0) {
    for (i in 1:length(listvar)) {
      if (!typeof(listvar[i])=="language") {listvar[i] <- parse(text=listvar[i])}
    }
  }  
  
  # create list with first vector to receive names
  varname <- as.character(listvar[1])
  
  if (length(listvar)>1) {
    # extract each name in turn and add to list
    for (i in 2:length(listvar)){
      varname <- c(varname, as.character(listvar[i]))
    }
  } 
  
  # use list of colunms to make table
  table(data[, varname], exclude=miss) 
  
}


