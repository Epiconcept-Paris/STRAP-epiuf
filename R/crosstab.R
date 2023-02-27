
# data = df
# var1 = "sari"
# var2 = "sari_who"
# missing = TRUE
# extra = "Total"

#' crosstab - outputs a cross table in format data frame
#'
#' @param data The data set to look at
#' @param var1 The first variable (the rows)
#' @param var2 The second variable (the columns)
#' @param missing Boolean, whether to show counts missing data. Default is True
#' @param extra What extra info you want. Currently default is 'Total' and output has row and column totals listed. There is space in the function for more kinds of extras to be added
#
crosstab <- function(data, var1, var2, missing=TRUE, extra="Total"){
  
  ctab <- data.frame(rbind(table(data[[var1]], data[[var2]], exclude=missing)))
  ncolum <- ncol(ctab)
  
  if(extra=="Total"){
  ctab$total <- rowSums(ctab[,1:ncolum])
  ctab <- rbind(ctab,c(colSums(ctab[,1:ncol(ctab[1:(ncolum+1)])]),""))
  }

  mynames <- c("No", "Yes", "Missing")
  
colnames(ctab) <- c(mynames[1:ncolum], extra)
rownames(ctab) <- c(mynames[1:ncolum], extra)
return(ctab)
}
