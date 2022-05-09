#
# Project Name : STRAP-epiuf
# Script Name  : descBy
# GitHub repo  : STRAP-epiuf
# Summary      : Descriptive table according to another variable
# Date created : 29/04/2022
# Author       : MML
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to describe variables according to another variable in a table.
# For example to describe age, sex, bmi by controls/cases status.
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' describBy
#'
#' Aim: to describe numeric variables or categorical variables (must be in factor) according to another variable in a table.
#' 
#' @param data name of the dataset
#' @param vars the variables list to describe (numeric or factor)
#' @param by variable to compare (must be a factor and have at least two categories)
#' @param labels labels for the variables list (var names used if NULL)
#' 
#' @return a descriptive table 
#' @export
#'
#' @examples
#' \dontrun{
#'    table <- descBy(data = df, vars = c("age","sex","fluvaccany"), 
#'    labels = c("Age", "Sex", "Seasonal influenza vaccination"), 
#'    by = "lab_flu"))
#' }
#' 
describBy <- function(data, vars, by, labels=NULL){
  ok <-  TRUE
  # Check if compVar is a factor
  if(class(data[[by]]) != "factor"){
    warning(by, " is not a factor")
    ok <-  FALSE
  }
  
  # Levels of compVar
  mod <- levels(data[[by]])
  # Check if there are at least 2 categories
  if(length(mod) < 2){
    warning(by, " has less than 2 categories")
    ok <-  FALSE
  }
  
  if (ok == TRUE) {
    # Select list of variables
    data.comp <- as.data.frame(data[,vars])
    if (is.null(labels)) labels <-  vars
    
  
    # For each level of compVar retrieve the number of records
    by <- as.data.frame(data[[by]])
    mod.nb <- rep(NA, length(mod))
    for (i in 1:length(mod)){
      mod.nb[i] <- nrow(subset(data.comp, by==mod[i]))
    }
    label.mod <- paste0(mod, "\n(N=", mod.nb, ")")
    
    # Empty object to save the table
    res <-c(1:(1+length(mod)))
    
    for(i in 1:length(vars))
    {
      # Numeric variable
      if (is.numeric(data.comp[,i]))
      {
        l1 <- labels[i]
        l2 <- "Mean (SD)"
        l3 <- "Median (IQR)"
        l4 <- "Missing, n(%)"
        
        for(k in 1:length(mod))
        {
          data.k <-as.data.frame(data.comp[by==mod[k] & !is.na(by),])
          n <- sum(!is.na(data.k[,i]))
          na <- sum(is.na(data.k[,i]))
          na.pct <- round(na/nrow(data.k)*100,1)
          moy <- round(mean(data.k[,i],na.rm=TRUE),1)
          sd <- round(sd(data.k[,i],na.rm=TRUE),1)
          med <- round(median(data.k[,i],na.rm=TRUE),1)
          p25 <- round(quantile(data.k[,i],c(0.25),na.rm=TRUE),1)
          p75 <- round(quantile(data.k[,i],c(0.75),na.rm=TRUE),1)
          
          l1 <- c(l1, NA)
          l2 <- c(l2, paste0(moy," (",sd,")"))
          l3 <- c(l3, paste0(med," (",p25,"-",p75,")"))
          l4 <- c(l4, paste0(na," (",na.pct,")"))
        }
        
        l1234 <- rbind(l1,l2,l3,l4)
        
        table.comp <- rbind(res,l1234)
        res <-table.comp
      }
      
      # Categorical variable
      if (is.factor(data.comp[,i]))
      {
        # Levels of the variable i 
        mod2 <- levels(data.comp[,i])
        
        # For each category/level j of the variable i
        for(j in 1:length(mod2))
        {
          l1 <- c(paste0(labels[i], ", n(%)"), rep(NA, length(mod)))
          l2 <- mod2[j]
          
          for(k in 1:length(mod)){
            data.k <- data.comp[by==mod[k] & !is.na(by),]
            n <- sum(!is.na(data.k[,i]))
            n.tab <- as.vector(table(data.k[,i]))
            n.pct <- round((n.tab/n)*100,1)
            
            if (j==1 & k!=length(mod)) 
            {
              l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
            }
            if (j==1 & k==length(mod))
            {
              l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
            }
            if (j>1 & k!=length(mod))
            {
              l1 <- l
              l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
            }
            if (j>1 & k==length(mod))
            {
              l1 <- l
              l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
            }            
          }
          l <- rbind(l1,l2)
        }
        
        l3 <- "Missing"
        
        for(k in 1:length(mod)){
          data.k <- data.comp[by==mod[k] & !is.na(by),]
          na <- sum(is.na(data.k[,i]))
          na.pct <- round(na/nrow(data.k)*100,1)
          l3 <- c(l3, paste0(na," (",na.pct,")"))
        }
        
        l <- rbind(l,l3)
        
        table.comp <- rbind(res,l)
        res <-table.comp
      }
    }
    
    table.comp <- table.comp[-1,]
    rownames(table.comp) <- 1:nrow(table.comp)
    colnames(table.comp) <- c("Variable", label.mod)
    table.comp <- as.data.frame(table.comp, row.names = 1:length(table.comp))
    return(table.comp)
  } else return(NULL)  
}


# END of SCRIPT  --------------------------------------------------------
