# Project Name : epiuf package
# Script Name  : to_drop
# GitHub repo  : 
# Summary      : 
# Date created : 2023-08-02
# Author       : Lore Merdrignac
# Date reviewed: 
# Reviewed by  : 

# Description -------------------------------------------------------------
# 

# Changes Log -------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


#' crosstab - outputs a cross table in format data frame  >> Obsolete replaced by epitable
#'
#' @param data The data set to look at
#' @param var1 Character string, colomn name of the first variable (the rows). 
#' Must be a 0/1 (numeric) or No/Yes (character) categorical variable (factors not supported yet).
#' @param var2 Character string, colomn name of the second variable (the columns).
#' Must be a 0/1 (numeric) or No/Yes (character) categorical variable (factors not supported yet).
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
  
  warning("The function crosstab has been depreciated and will be deleted from the Epiuf package.\nPlease use the function epiuf::epitable()")
  
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
    # PR_LMC Removing 100% percent from the sapply to avoid summing it up 
    # ctab <- rbind(ctab, sapply(ctab[nrows+1,], function(x) round(x/total*100,decimals)))
    ctab <- rbind(ctab,c(sapply(ctab[nrows+1,-(ncolum+2)], function(x) round(x/total*100,decimals)), 100))
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



#' moyenne d’un vecteur >> Obsolete replaced by getMean
#' 
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @importFrom stats na.omit
#' @export
moyenne <- function(x){
  # x <-  na.omit(x)
  # sum(x)/length(x)
  stop("The function moyenne has been depreciated and will be deleted from the Epiuf package.\nPlease use the function epiuf::getMean()")
}


#' comp  >> Obsolete replaced by desc and desc by
#' 
#' Aim : To describe numeric variables or categorical variables (need to be in factor) 
#' according to an another variable
#' 
#' @param vars list of vars you want to describe
#' @param comp.var variable to compare (need to be data$var)
#' @param labels labels for the list of vars
#' @param data name of the dataset
#'
#' @return the result 
#' @export
#' @importFrom stats median quantile
#'
#' @examples
#' data <- data.frame(Id = 1:4 , 
#'                     Vaccs = c("pfizer,moderna"," ", "pfizer", "moderna"),
#'                     Cov = 1,0,1,0)
#' vars <- c("Id", "Vaccs")
#' labels <- c("Id number", "Vaccination Brand")
#' \dontrun{
#' tab.comp1 <- comp(vars,data$Cov , labels, data) 
#' }
comp <- function(vars, comp.var, labels, data)
{
  # # Select list of variables
  # data.comp <- data[,c(vars)]
  # 
  # # Levels of comp.var
  # mod <- levels(comp.var)
  # mod.nb <- rep(NA, length(mod))
  # for (i in 1:length(mod)){
  #   mod.nb[i] <-nrow(subset(data.comp, comp.var==mod[i]))
  # }
  # label.mod <- paste0(mod, "\n(N=", mod.nb, ")")
  # 
  # # Empty object to save the table
  # res <-c(1:(1+length(mod)))
  # 
  # for(i in 1:length(vars))
  # {
  #   # Numeric variable
  #   if (is.numeric(data.comp[,i]))
  #   {
  #     l1 <- labels[i]
  #     l2 <- "Mean (SD)"
  #     l3 <- "Median (IQR)"
  #     l4 <- "Missing, n(%)"
  #     
  #     for(k in 1:length(mod))
  #     {
  #       data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
  #       n <- sum(!is.na(data.k[,i]))
  #       na <- sum(is.na(data.k[,i]))
  #       na.pct <- round(na/nrow(data.k)*100,1)
  #       moy <- round(mean(data.k[,i],na.rm=TRUE),1)
  #       sd <- round(sd(data.k[,i],na.rm=TRUE),1)
  #       med <- round(median(data.k[,i],na.rm=TRUE),1)
  #       p25 <- round(quantile(data.k[,i],c(0.25),na.rm=TRUE),1)
  #       p75 <- round(quantile(data.k[,i],c(0.75),na.rm=TRUE),1)
  #       
  #       l1 <- c(l1, NA)
  #       l2 <- c(l2, paste0(moy," (",sd,")"))
  #       l3 <- c(l3, paste0(med," (",p25,"-",p75,")"))
  #       l4 <- c(l4, paste0(na," (",na.pct,")"))
  #     }
  #     
  #     l1234 <- rbind(l1,l2,l3,l4)
  #     
  #     table.comp <- rbind(res,l1234)
  #     res <-table.comp
  #   }
  #   
  #   # Categorical variable
  #   if (is.factor(data.comp[,i]))
  #   {
  #     # Levels of variable i 
  #     mod2 <- levels(data.comp[,i])
  #     
  #     for(j in 1:length(mod2))
  #     {
  #       l1 <- c(paste0(labels[i], ", n(%)"), rep(NA, length(mod)))
  #       l2 <- mod2[j]
  #       
  #       for(k in 1:length(mod)){
  #         data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
  #         n <- sum(!is.na(data.k[,i]))
  #         n.tab <- as.vector(table(data.k[,i]))
  #         n.pct <- round((n.tab/n)*100,1)
  #         
  #         if (j==1 & k!=length(mod)) 
  #         {
  #           l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
  #         }
  #         if (j==1 & k==length(mod))
  #         {
  #           l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
  #         }
  #         if (j>1 & k!=length(mod))
  #         {
  #           l1 <- l
  #           l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
  #         }
  #         if (j>1 & k==length(mod))
  #         {
  #           l1 <- l
  #           l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
  #         }            
  #       }
  #       l <- rbind(l1,l2)
  #     }
  #     
  #     l3 <- "Missing"
  #     
  #     for(k in 1:length(mod)){
  #       data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
  #       na <- sum(is.na(data.k[,i]))
  #       na.pct <- round(na/nrow(data.k)*100,1)
  #       l3 <- c(l3, paste0(na," (",na.pct,")"))
  #     }
  #     
  #     l <- rbind(l,l3)
  #     
  #     table.comp <- rbind(res,l)
  #     res <-table.comp
  #   }
  # }
  # 
  # table.comp <- table.comp[-1,]
  # rownames(table.comp) <- 1:nrow(table.comp)
  # colnames(table.comp) <- c("Variable", label.mod)
  # return(table.comp)
  stop("The function comp has been depreciated and will be deleted from the epiuf package.\nPlease use function epiuf::descBy()")
}


# END of SCRIPT  ----------------------------------------------------------