#' checkMissing
#'
#'
#' @param what a data.frame, a vector or a vector of colnames
#' @param ...  a list of variable to be tested. Is used only if "what" is not a dataframe
#' @param sort boolean If TRUE the output table is sorted 'NOT IMPLEMENTED'
#' @param showall boolean if FALSE, the default, then variables with no missing values are
#' not included in the table
#'
#' @return list of 2 tables
#' @export checkmissing
#'
#' @author Gilles Desve
#'
#' @examples
#' # still nothing
checkmissing <- function(data, varlist =NULL, sort=FALSE,showall=FALSE ) {

  if (is.null(varlist)) {
    varlist = as.list(names(data))
  } else showall <- TRUE
  # one varname or a list ? 
  if ( ! is.list(varlist)) varlist <- as.list(varlist)
  nb <- length(varlist)
  

  # all this as to be restructured to accept vector (and df to be used as df[i]) XXX
  effectif = nrow(data);  # var1 if single var
  
  res1  = data.frame();
  i <- 0
  nomiss <- c()
  for (name in varlist) {
    i <- i + 1
    if (any(name==colnames(data)) ) {
      miss1 <- sum(is.na(data[,name]))
      if (miss1 > 0 | showall ) {
        pmiss1 <- round((miss1 / effectif) * 100, digits = 2)
        res1 <- rbind(res1, c(miss1,pmiss1))
      } else {
        nomiss <- cbind(nomiss,i)
      }
    } else {
      res1 <- rbind(res1, c(NA,NA))
    }
  }
  if ( length(nomiss) > 0 ) {
    varlist <- varlist[-nomiss]
  }
  varlist <- lapply(varlist,lpad,width=16)
  rownames(res1) <- varlist
  colnames(res1) <- c("Nb_Missing", "% Missing")
  names(dimnames(res1)) <- c("variables","Missing")
  if (sort == TRUE) {
    res1 <- sortBy(res1,1)
  }
  return(res1);
  
  # missbyrow <- function(vars, effectif) {
  #   counts = apply(df, 1, function(x) sum(is.na(x[vars])));
  #   d <- as.data.frame(counts);
  #   t <- table(counts);
  #   vals = names(t);
  #   res1 <- cbind(t, round(prop.table(t)*100,2));
  #   Cum <- cumsum(res1[,2]);
  #   res2 <- cbind(vals, res1[,1], res1[,2], Cum);
  #   names(res2) <- c("Nb missing values", "Frequency", "Percent", "Cumul")
  #   return(res2)
  # }
  #
  # res1 <- missdesc(vars, effectif)
  # res2 <- missbyrow(vars, effectif)
  # result <- list(missing=res1, missbyrow=res2)
  # result
  
}
