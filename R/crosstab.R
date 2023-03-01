
# data = df
# var1 = "sari"
# var2 = "sari_who"
# missing = TRUE
# extra = "Total"

#' crosstab - outputs a cross table in format data frame
#'
#' @param data The data set to look at
#' @param var1 Character string, colomn name of the first variable (the rows)
#' @param var2 Character string, colomn name of the second variable (the columns)
#' @param missing takes auguments 'no' to not show missing and 'always' to show missing (as for table())
#' @param decimals state how many decimal points you want to list for the output of any calculations. Default is 1
#' @param extra What extra info you want. Current options "None", "Total" and "Percent". Default is "None". Feel free to add your own!
#' 
#' @examples 
#' data <- data.frame(id = 1:10,
#'                    cases = c(rep(1,3), rep(0,7)),
#'                    vacc = sample(c(0,1), replace = TRUE, size = 10))
#' table(data$cases, data$vacc, useNA = "always")
#' crosstab(data, 
#'          var1 = "cases", 
#'          var2 = "vacc")
#' crosstab(data, 
#'          var1 = "cases", 
#'          var2 = "vacc", 
#'          missing = "no")
#' crosstab(data, 
#'          var1 = "cases", 
#'          var2 = "vacc", 
#'          extra = "Total")
#' crosstab(data, 
#'          var1 = "cases", 
#'          var2 = "vacc", 
#'          extra = "Percent")
#' 
#' @export

crosstab <- function(data, var1, var2, missing="always", extra="None", decimals=1){
  
  ctab <- data.frame(rbind(table(data[[var1]], data[[var2]], useNA=missing)))
  ncolum <- ncol(ctab)
  nrows <- nrow(ctab)
  total <- nrow(data)
  
  if(extra=="Total"|extra=="Percent"){
    ctab$total <- rowSums(ctab[,1:ncolum])
    ctab <- rbind(ctab,c(colSums(ctab[,1:ncol(ctab[1:(ncolum+1)])])))
    extraname <- extra
  }
  
  if(extra=="Percent"){
    ctab$percent <- round(ctab$total/total*100,decimals)
    ctab <- rbind(ctab, sapply(ctab[nrows+1,], function(x) round(x/total*100,decimals)))
    ctab$total <- NULL
    ctab<-ctab[-(nrows+1),]
  }
  
  if(extra=="None"){extraname <- NULL}
  
  mynames <- c("No", "Yes")
  if(missing=="always"){mynames<- c(mynames, "Missing")}
  
  
  colnames(ctab) <- c(mynames[1:ncolum], extraname)
  rownames(ctab) <- c(mynames[1:ncolum], extraname)
  return(ctab)
}
