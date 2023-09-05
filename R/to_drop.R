#' Project Name : epiuf package
#' Script Name  : to_drop
#' GitHub repo  : 
#' Summary      : 
#' Date created : 2023-08-02
#' Author       : Lore Merdrignac
#' Date reviewed: 
#' Reviewed by  : 

#' Description -------------------------------------------------------------
#' 

#' Changes Log -------------------------------------------------------------
#' 

# START of SCRIPT  --------------------------------------------------------


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