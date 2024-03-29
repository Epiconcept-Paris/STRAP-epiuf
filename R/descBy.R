#
# Project Name : STRAP-epiuf
# Script Name  : descBy
# GitHub repo  : STRAP-epiuf
# Summary      : Descriptive table according or not to another variable
# Date created : 29/04/2022
# Author       : MML
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to describe variables according or not to another variable in a table.
# For example to describe age, sex, bmi or to describe age, sex, bmi by controls/cases status.
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

describe <- function(data.desc){
  
  # Numeric variable
  if (is.numeric(data.desc)){
    
    n <- sum(!is.na(data.desc))
    na <- sum(is.na(data.desc))
    na.pct <- round(na/length(data.desc)*100,1)
    moy <- round(mean(data.desc,na.rm=TRUE),1)
    sd <- round(sd(data.desc,na.rm=TRUE),1)
    med <- round(median(data.desc,na.rm=TRUE),1)
    p25 <- round(quantile(data.desc,c(0.25),na.rm=TRUE),1)
    p75 <- round(quantile(data.desc,c(0.75),na.rm=TRUE),1)
    
    # Variable label
    l1 <- NA
    # Mean (SD)
    l2 <- paste0(moy," (",sd,")")
    # Median (IQR)
    l3 <- paste0(med," (",p25,"-",p75,")")
    # Missing, n(%)
    if(na == 0){
      l4 <- na
    }else{
      l4 <- paste0(na," (",na.pct,")")
    }
    
    table.desc <- rbind(l1,l2,l3,l4)
  }
  
  
  # Categorical variable
  if (is.factor(data.desc)){
    
    # Levels of the variable
    mod.var <- levels(data.desc)
    
    n <- sum(!is.na(data.desc))
    na <- sum(is.na(data.desc))
    na.pct <- round(na/length(data.desc)*100,1)
    n.tab <- as.vector(table(data.desc))
    n.pct <- round((n.tab/n)*100,1)
    
    # Variable label
    l1 <- NA
    
    # For each level j of the variable
    for(j in 1:length(mod.var)){
      
      l2 <- paste0(n.tab[j]," (",n.pct[j],")")
      l12 <- rbind(l1,l2)
      l1 <- l12
    }
    
    # n(%) x nb of variable levels (1 line = 1 level)
    l123 <- l12
    # Missing, n(%)
    if(na == 0){
      l4 <- na
    }else{
      l4 <- paste0(na," (",na.pct,")")
    }
    table.desc <- rbind(l123,l4)
  }
  return(table.desc)
}


#' descBy
#'
#' Describe numeric variables or categorical variables (must be in factor) according or not to another variable in a table.
#' 
#' @param data name of the dataset
#' @param vars the variables list to describe (numeric or factor)
#' @param labels labels for the variables list
#' @param by variable to compare (must be a factor and have at least two categories)
#' 
#' @return a descriptive table 
#' @export
#'
#' @examples
#'  descBy(iris, "Sepal.Length", by = "Species") 
#'
#' 
descBy <- function(data, vars, labels=NULL, by = NULL){
  # Build the simple descriptive table
  data <- as.data.frame(data)
  if (is.null(labels)) labels <-  vars

    if(is.null(by)){
    # Select list of variables
    data.subset <-data[,vars]
    
    # Empty object to save the table
    table <- NULL
    
    # Loop in the list of variables to be described
    for (i in 1:length(vars)){
      
      var <- data.subset[,i]
      
      # Numeric variable
      if(is.numeric(var)){
        
        l1 <- labels[i]
        l2 <- "Mean (SD)"
        l3 <- "Median (IQR)"
        l4 <- "Missing, n(%)"
        
        l1234 <- rbind(l1,l2,l3,l4)
      }
      
      # Categorical variable
      if (is.factor(var)){
        
        mod.var <- levels(var)
        
        l1 <- labels[i]
        
        for(k in 1:length(mod.var)){
          l2 <- paste0(mod.var[k],", n(%)")
          l12 <- rbind(l1,l2)
          l1 <- l12
        }
        
        l4 <- "Missing, n(%)"
        l1234 <- rbind(l1, l4)
        
      }
      # Merge labels and values
      res <- rbind(table, cbind(l1234, describe(var)))
      table <- res
    }
    
    colnames(table) <- c("Variables", "Description")
    table <- as.data.frame(table, row.names = 1:nrow(table))
    return(table)
    
  }else{
    
    # Select list of variables
    data.subset <- data[,c(vars,by)]
    
    # Check if by is a factor
    if( ! is.factor(data[,by])){
      stop(by, " is not a factor")
    }
    
    # Levels of by variable
    mod.by <- levels(data[,by])
    # Check if there are at least 2 categories
    if(length(mod.by) < 2){
      stop(by, " has less than 2 categories")
    }
    
    # For each level of by variable retrieve the number of records
    mod.by.nb <- rep(NA, length(mod.by))
    for (i in 1:length(mod.by)){
      mod.by.nb[i] <- nrow(data.subset[data.subset[[by]] == mod.by[i],])
    }
    mod.by.label <- paste0(mod.by, "\n(N=", mod.by.nb, ")")
    
    # Empty object to save the table
    table <- NULL
    
    # Loop in the list of variables to be described
    for (i in 1:length(vars)){
      
      var <- data.subset[,i]
      
      # Numeric variable
      if(is.numeric(var)){
        
        l1 <- labels[i]
        l2 <- "Mean (SD)"
        l3 <- "Median (IQR)"
        l4 <- "Missing, n(%)"
        
        l1234 <- rbind(l1,l2,l3,l4)
        
        all.res <- NULL
        for(k in 1:length(mod.by)){
          data.k <- data.subset[data.subset[[by]] == mod.by[k],]
          res <- describe(data.k[,vars[i]])
          all.res <- cbind(all.res, res)
        }
      }
      
      # Categorical variable
      if (is.factor(var)){
        
        mod.var <- levels(var)
        
        l1 <- labels[i]
        
        for(k in 1:length(mod.var)){
          l2 <- paste0(mod.var[k],", n(%)")
          l12 <- rbind(l1,l2)
          l1 <- l12
        }
        
        l4 <- "Missing, n(%)"
        l1234 <- rbind(l1, l4)
        
        all.res <- NULL
        for(k in 1:length(mod.by)){
          data.k <- data.subset[data.subset[[by]] == mod.by[k],]
          res <- describe(data.k[,vars[i]])
          all.res <- cbind(all.res, res)
        }
      }
      
      # Merge labels and values
      res <- rbind(table, cbind(l1234, all.res))
      table <- res
    }
    
    rownames(table) <- 1:nrow(table)
    colnames(table) <- c("Variable", mod.by.label)
    table <- as.data.frame(table)
    return(table)
  }
}


# END of SCRIPT  --------------------------------------------------------
