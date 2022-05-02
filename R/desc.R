#
# Project Name : STRAP-epiuf
# Script Name  : desc
# GitHub repo  : STRAP-epiuf
# Summary      : Descriptive table
# Date created : 29/04/2022
# Author       : MML
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to describe variables in table.
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' desc
#'
#' Aim: to describe numeric variables or categorical variables (must be in factor) in a table.
#' 
#' @param data name of the dataset
#' @param vars the variables list to describe (numeric or factor)
#' @param labels labels for the variables list
#' 
#' @return a descriptive table 
#' @export
#'
#' @examples
#' \dontrun{
#'    table <- desc(data = df, vars = c("age","sex","fluvaccany"), 
#'    labels = c("Age", "Sex", "Seasonal influenza vaccination") )
#' }
#' 
desc <- function(data, vars, labels=NULL){
  data.desc <- data[,vars]
  res <-c(1:3)
  lvide <- c("","","")
  if (is.null(labels)) labels <-  vars
  
  for(i in 1:length(vars))
  {
    # Numeric variable
    if (is.numeric(data.desc[,i]))
    {
      n <- sum(!is.na(data.desc[,i]))
      na <- sum(is.na(data.desc[,i]))
      moy <- round(mean(data.desc[,i],na.rm=TRUE),1)
      sd <- round(sd(data.desc[,i],na.rm=TRUE),1)
      med <- round(median(data.desc[,i],na.rm=TRUE),1)
      p25 <- round(quantile(data.desc[,i],c(0.25),na.rm=TRUE),1)
      p75 <- round(quantile(data.desc[,i],c(0.75),na.rm=TRUE),1)
      min <- round(min(data.desc[,i],na.rm=TRUE),1)
      max <- round(max(data.desc[,i],na.rm=TRUE),1)
      
      l1 <- c(paste(labels[i],sep=""),na,paste("n=",n))
      l2 <- c("Mean (SD)","",paste(moy," (",sd,")",sep="")) # only if you want mean and SD
      l1234 <- rbind(l1,l2)
      #l3 <- c("Median (IQR)",NA,paste(med," (",p25,"-",p75,")",sep=""))
      #l4 <- c("Min, Max",NA,paste(min,", ",max,sep=""))
      #l1234 <- rbind(l1,l2,l3,l4)
      
      
      table.desc <- rbind(res,l1234,lvide)
      res <-table.desc
    }
    # Categorical variable
    if (is.factor(data.desc[,i]))
    {
      n <- sum(!is.na(data.desc[,i]))
      na <- sum(is.na(data.desc[,i]))
      n.tab <- as.vector(table(data.desc[,i]))
      n.pct <- round((n.tab/n)*100,2)
      mod <- levels(data.desc[,i])
      
      l1 <- c(paste(labels[i],sep=""),na,paste("n=",n))
      for(k in 1:length(mod))
      {
        l2 <- c(paste(mod[k],", n(%)",sep=""),"",paste(n.tab[k]," (",n.pct[k],")",sep=""))
        l <- rbind(l1,l2)
        l1<- l
      }
      l123 <- l
      table.desc <- rbind(res,l123,lvide)
      res <-table.desc
      
    }
  }
  table.desc <- table.desc[-1,]
  colnames(table.desc)=c("Variables","N missing","Description")
  table.desc <- as.data.frame(table.desc, row.names = 1:length(table.desc))
  return(table.desc)
}


# END of SCRIPT  --------------------------------------------------------
