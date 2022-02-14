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
