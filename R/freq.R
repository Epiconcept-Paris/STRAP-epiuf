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
#' @param y As numbers, factors or text.
#' @param missing If false then missing values are not included in the table
#'   A summary output of number of missing values is added at the end
#' @param quietly No output, only return value
#' @return An array containing  values of \code{...}   \code{}
#'
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'
freq <- function(x,y=NULL,missing=FALSE,quietly = FALSE) {
  
  r <- try(class(x),TRUE)
  if ( ! inherits(r, "try-error")) {
    if ("data.frame" %in% r ) {
      var.name <- deparse(substitute(y))
      ys <- parse(text=substitute(y))
      y <-  eval(ys,x) 
    } else if (is.character(x) & length(x)==1 ) {
      var.name <- x
      y  <- getvar(x) 
    } else {
      var.name <- deparse(substitute(x))
      y <- x 
    }
  } else {
    var.name <- deparse(substitute(x))
    y <- getvar(var.name)
  } 
  
  cur.var <- y
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



fmtpval <-function(pvalue,digits) {
  res <- round(pvalue,digits)
  if (res==0) {
    res <- paste0("< ",format(1/10^digits,scientific=FALSE) )
  }
  res
}


tab_line <- function(ncol, tot = FALSE, first=FIRST) {
  l1 <- replicate(LINE, first + 1)
  l2 <- replicate(LINE, (ncol - 1) * (COL + 2))
  l3 <- ifelse(tot, CROSS, LINE)
  l4 <- replicate(LINE, COL )
  cat(l1, CROSS, l2, l3, l4, "\n", sep = "")
}

tab_row <- function(rname, line, deci=0, tot = FALSE, perc = FALSE, coldeci=NULL, indic=NULL, first=FIRST) {
  l <- length(line)
  if (is.null(coldeci)) {coldeci[1:l] <- FALSE}
  cat(lpad(rname, first))
  cat("", SEP)
  for (i in 1:(l - 1)) {
    ndigit <- ifelse(coldeci[i],deci,0)
    value <- line[[i]]
    if(is.na(value)) value <-"" 
    fout <- lpad(value, COL, digit = ndigit)
    cat(fout, " ")
  }
  if (tot) {
    if (!is.null(indic)) {
      cat(indic)
    } else {
      cat(SEP)
    }
  }
  ndigit <- ifelse(coldeci[l],deci,0)
  cat(lpad(line[[l]], COL, ndigit ))
  
  if (!tot & perc) {
    if (!is.null(indic)) {
      cat("",indic)
    } else {
      cat("",SEP)
    }
  }
  cat("\n")
  
}


replicate <- function(char, ntime) {
  paste(rep(char, ntime), collapse = "")
}


insertrow <- function(DFtoadd, newrow, r) {
  DFtoadd  <- rbind(DFtoadd[1:r-1], newrow, DFtoadd[-(1:r-1)])
}


outputtable <-
  function(table,
           deci = NULL,
           totcol = FALSE,
           totrow = TRUE,
           title = "Frequency distribution",
           rowperc = NULL,
           colperc=NULL,
           totperc=NULL,
           coldeci=NULL,
           first=FIRST)  {
    catret(title)
    catret("")
    ncol <- dim(table)[2]
    nline <- dim(table)[1]
    coln <- colnames(table)
    rown <- rownames(table)
    
    if ( ! is.null(colperc) & totrow ) { colperc <- cbind(colperc, c(rep("",nrow(colperc)-1),100))} # add extra row to be able to format table output properly
    
    
    if (is.null(coldeci)) {
      coldeci[1:ncol] <- FALSE
    }
    
    # columns title
    if (! is.null(names(dimnames(table))[2]) ) {
      catret(replicate(" ",COL*(ncol/2)+FIRST ), names(dimnames(table))[2])
    }
    
    # rows title and columns names
    name <- names(dimnames(table))[1]
    if (is.null(name))  name <- ""
    tab_row(name, coln, deci, totcol, perc = FALSE, coldeci,first=first)
    
    # separator line
    tab_line(ncol, totcol,first=first)
    
    percdeci<-NULL
    percdeci[1:ncol-1] <- TRUE
    percdeci[ncol] <- FALSE
    
    # each row
    totline <- nline
    if (totrow) {totline <- nline - 1}
    for (i in (1:(totline))) {
      tab_row(rown[i], table[i, ], deci, totcol,perc = FALSE, coldeci,first=first)
      if ( ! is.null(totperc) ) {
        tab_row("", totperc[i, ], deci, totcol,perc = TRUE, percdeci,indic="O",first=first)
      }
      if ( ! is.null(rowperc) ) {
        tab_row("", rowperc[i, ], deci, totcol,perc = TRUE,percdeci,indic=">",first=first)
      }
      if ( ! is.null(colperc) ) {
        tab_row("", colperc[i, ], deci, totcol,perc = TRUE,percdeci,indic="V",first=first)
      }
    }
    
    # separator line
    tab_line(ncol, totcol,first=first)
    # Totals row
    if (totrow) {
      tab_row(rown[nline], table[nline, ], deci, totcol, perc = FALSE, coldeci,first=first)
      if ( ! is.null(totperc) ) {
        tab_row("", totperc[nline, ], deci, totcol,perc = FALSE,percdeci,indic="O",first=first)
      }
      if ( ! is.null(colperc) ) {
        tab_row("", colperc[nline, ], deci, totcol,perc = FALSE, percdeci,indic="V",first=first)
      }
    }
  }




# epifield documentation for RData using roxygen2
#' @title
#' Cross tabulation ( 2by2 table).
#' @description
#' \code{epitable} Display a cross tabulation of two variables optionnaly with
#'  row or col percentages. Chi Square with associated p.value are calculated.
#'  If table contain binary variable, then epiorder function is apply on the two variable
#'  to get a resulting table compatible with usual epidemiology interpretation.
#'  0/1 variables are transformed into Yes/No and Yes is displayed before No
#'  Exposed Cases appear on upper left part of the table.
#'
#'
#' @name epitable
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{freq}} for frequency distributions
#' @importFrom stats chisq.test fisher.test
#' @export
#' @param data The dataframe to be analysed
#' @param out  "Outcome" as numbers, factors or text
#' @param exp  "Exposure" as numbers, factors or text. short syntax is available
#' see help(epifield)
#' @param epiorder Should data be reordered to respect epi tables , default to TRUE
#' @param missing Boolean if FALSE, missing are not included in the table.
#'   A summary output of number of missing values is added at the end
#' @param row  "Row percentages"
#' @param col  "Col percentages"
#' @param perc "Table percentages"
#' @param fisher TRUE by default, display the fisher exact probability.
#' If table is larger than 2*2 then Fisher is not calculated
#' @param total Default TRUE , display marginal total
#' 
#' 
#' @return An array containing  values of \code{
#' table : The resulting table
#' rowperc : The optional row percentage
#' colperc : The optional col percentage  
#' chisq : Chi Square value
#' p : Estimated probability of this distribution
#' fischer : Exact probaility
#' missing : Number of missing values

#' 
#' }   
#' 
#' @examples
#' data <- data.frame(id = 1:10,
#'                    cases = c(rep(1,3), rep(0,7)),
#'                    vacc = sample(c(0,1), replace = TRUE, size = 10))
#' data[8,2]<-NA                    
#' table(data$cases, data$vacc, useNA = "always")
#' result <- epitable(data,cases,vacc)
#' epitable(data,cases,vacc,epiorder=FALSE)
#' 
#' epitable(data,"cases","vacc") 
#' epitable(data,out=cases,exp=vacc,missing=TRUE) 
#' epitable(data,out=cases,exp=vacc,row=TRUE) 
#' 
#' table <- result
#' result$table
#' 
#' data <- data.frame(id = 1:10,
#'                    cases = c(rep(1,3), rep(0,7)),
#'                    vacc = sample(c(0,1,2), replace = TRUE, size = 10))
#' epitable(data,cases,vacc,perc=TRUE)
#'


epitable <- function(data,out,exp,epiorder=TRUE,missing=FALSE,row=FALSE,col=FALSE,perc = FALSE, fisher=TRUE,total=TRUE)  {
  r <- try(class(data),TRUE)
  if ( ! inherits(r, "try-error")) {
    if ("data.frame" %in% r ) {
      out.name <- deparse(substitute(out))
      exp.name <- deparse(substitute(exp))
      out <- parse(text=substitute(out))
      out <-  eval(out,data) 
      exp <- parse(text=substitute(exp))
      exp <-  eval(exp,data)
    } else { 
      stop(paste("data must be a dataframe")) 
    }
  }
  #   if (class(out)=="character" & length(out)==1 ) {
  #   var.out <- out
  #   y  <- getvar(x) 
  # } else {
  #   var.name <- deparse(substitute(x))
  #   y <- x 
  # }
  # } else {
  #   var.name <- deparse(substitute(x))
  #   y <- getvar(var.name)
  # } 
  
  # r <- as.list(match.call())
  # expdata <- getvar(r$exp)
  # expdata.name <- as.character(substitute(exp)) # getvarname()
  # expdata.fname <- getvar()
  # if (! is.null(expdata)) {
  #   expdata <- epiorder(expdata,update=FALSE,reverse=TRUE )
  # }
  
  tot <- length(exp)
  
  # outdata <- getvar(r$out)
  # outdata.name <- as.character(substitute(out)) #getvarname()
  # outdata.fname <- getvar()
  # if ( ! is.null(outdata)) {
  #   outdata <- epiorder(outdata,update=FALSE, reverse=TRUE)
  # }
  # length to be verified
  
  if (! length(out) == tot) {
    stop(paste("all arguments must have the same length",out.name,"<>",exp.name,
               "verify that data comes from same database" ))
  }
  
  # to get options
  params <- names(data)
  
  if (! ( is.null(exp) |  is.null(out) )  ) {
    if (epiorder) {
      out <- epiorder(out,reverse=TRUE)
      exp <- epiorder(exp,reverse=TRUE)
    }
    # calculations
    r <- table(exp,out,useNA=ifelse(missing,"ifany","no"))
    if(missing){
      colnames(r)[which(is.na(colnames(r)))] <- "Missing"
      rownames(r)[which(is.na(rownames(r)))] <- "Missing"
    }
    # make a table with no missing so can run a fishers yet still output missing
    r_nomiss <- table(exp,out,useNA="no") 
    # to suppress the chisq warning if table is more than 2*2
    options("warn"=-1)
    t <- chisq.test(r) # best to run chi square with the missing listed or not??
    options("warn"=0)
    # check size of result table
    bin <- (dim(r_nomiss)[1]==2 & dim(r_nomiss)[2]==2)
    if (bin & fisher) {
      f <- fisher.test(r_nomiss)$p.value
    } else {fisher <- FALSE}
    proprow <- NULL # Why do you have these here?
    propcol <- NULL
    proptot <- NULL
    if (row) {
      proprow <- round(prop.table(r,1)*100, digits = 2)
      if(total){
        proprow <- cbind(proprow,100)
        colnames(proprow) <- c(colnames(proprow)[1:(length(colnames(proprow))-1)], "Total")
      }
    }
    if (col) {
      propcol <- round(prop.table(r,2)*100, digits = 2)
      if(total){
        #propcol <- cbind(propcol,"") # why this line?
        propcol <- rbind(propcol,100)
        rownames(propcol) <- c(rownames(propcol)[1:(length(rownames(propcol))-1)], "Total")
      }
    }
    if (perc){
      proptot <- round(prop.table(r)*100, digits = 2)
      if(total){
        proptot <- rbind(proptot,sapply(1:ncol(proptot), function(x) sum(proptot[,x])))
        proptot <- cbind(proptot,sapply(1:nrow(proptot), function(x) sum(proptot[x,])))
        colnames(proptot) <- c(colnames(proptot)[1:(length(colnames(proptot))-1)], "Total")
        rownames(proptot) <- c(rownames(proptot)[1:(length(rownames(proptot))-1)], "Total")
      }
    }
    
    if(total) {
      m <- margin.table(r,1)
      r <- cbind(r,Total = m)
      m <- margin.table(r,2)
      r <- rbind(r,Total = m)
    }
    # must be done after all structure changes
    names(dimnames(r))  <- c(exp.name,out.name)
    
    mis  <- sum(is.na(exp)|is.na(out))
    
    title <- paste("Tabulation of",out.name,"by",exp.name)
    
    outputtable(r, deci=1, totcol=total, totrow = total, title=title, rowperc = proprow , colperc = propcol , totperc=proptot)
    
    # construct the return list
    result <- list()
    result$table <- r
    result$rowperc <- proprow
    result$colperc <- propcol
    result$totperc <- proptot
    
    result$chisq <- t$statistic[[1]]
    result$chisq.p <- t$p.value
    #result$fischer <- t$p.value # why use chi squared here and not fisher?
    result$fischer <- ifelse(fisher, f, "Fischers not done") # suggest this alernative to avoid confusion that fishers was done when it wasnt
    result$missing <- mis
    
    # print stat result for interactive mode
    catret("")
    cat("Chi2:",t$statistic,"(", fmtpval(t$p.value,digits = getEpiOption("stat_digits")),")" )
    if (fisher) {
      
      cat(" Fisher exact :",fmtpval(f,digits = getEpiOption("stat_digits")))
    }
    catret("")
    if (!missing) {
      catret("Missing (NA):",mis," (",round(mis/tot*100, digits = 2),"%)\n")
    }
    # print(result$table)
    invisible(result)
  }
}


# epifield documentation using roxygen2
#' @title
#' Reorder data for epi table ( 2by2 table).
#' @description
#' \code{epiorder} Rearrange order of factor variable to produce classical epi table
#'  1/0  Yes/No  +/-
#'
#'
#' @name epiorder
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{epitable}} for cross tabulation
#' @export
#' @param var  Variable to reorder (will be converted as factor).
#' @param mode Label plan for the new factor variable
#'         "yesno" for Yes, No
#'         "10"    for 1, 0
#'         "+-"    for +,-
#'         "truefalse"  for TRUE, FALSE
#'         or any set of two labels on the form c("A","B")
#' @param levels  Custom set of levels as vector : c(1,0)
#'        This levels will replaced by the labels. Levels and labels should have the same
#'        length and the same order
#' @param update if TRUE (the default) Then the original dataset is updated with new value
#'        The recoded column is replaced by the returned value
#'        With this option, there is no need to reassign the retruned value,
#'        but original data are lost.
#' @param reverse if TRUE labels are reordered to better fit epidemiological tables
#'        with 1 before 0 , Yes before No etc...
#'        Other type of label are not changed, existing factor are not changed
#'
#' @return A vector reordered Can be nested as in \code{freq(epioredr(case))}
#' @examples
#' \dontrun{
#' epiorder(c(0,1,0,1,1))
#' }
#'
#'
epiorder <- function(var,
                     mode = "yesno",
                     levels = NULL,
                     update = TRUE,
                     reverse = FALSE) {
  r <- as.list(match.call())
  coldata <- var
  colname <- as.character(substitute(var))
  lab <- NULL
  if (!is.null(coldata)) {
    if (length(mode) > 1 & is.character(mode)) {
      lab <- mode
    } else {
      switch(
        mode ,
        "yesno" = {
          lab <- c("No","Yes")
        } ,
        "auto" = {
          lab <- ""
        } ,
        "10" = {
          lab <- c("0","1")
        } ,
        "+-" = {
          lab <- c("-","+")
        } ,
        "truefalse" = {
          lab <- c("FALSE","TRUE")
        } ,
        {
          cat("Mode:", mode, " Incorrect. See help(epiorder)")
          lab <- NULL
        }
      )
    }
    
    if (!is.null(lab)) {
      
      fvar <- is.factor(coldata)
      if ( fvar ) {
        if (is.null(levels)) {
          clevels <- levels(coldata)
          nlevels <- nlevels(coldata)
          if (nlevels == 2 & (substr(toupper(sort(clevels)[1]),1,1) == "N" ) ){
            clevels <- clevels
          } else if  (nlevels == 2 & (substr(toupper(sort(clevels)[1]),1,1) == "0" ) ) {
            mode="10"
            lab <- c("0","1")
          }  else {
            lab <- NULL
          }
        } else {
          clevels <- levels
          reverse <- FALSE
        }
      } else {
        coldata <- factor(coldata)
        # test for type of levels  (otherwise calling it two time will erase all values)
        clevels <- levels(coldata)
        nlevels <- nlevels(coldata)
        if (is.null(levels)) {
          if (nlevels == 2 ) {
            first <- sort(clevels)[1]
            if (first == "0") {
              clevels <- c(0,1)
            } else if ( substr(toupper(sort(clevels)[1]),1,1) == "N" ) {
              clevels <- sort(clevels)
            } else {
              lab <- NULL
            }
          } else if (nlevels > 2) {
            if (mode == "yesno" ) {
              # we keep base factor
              lab <- NULL
            } else if (nlevels == length(mode) & nlevels == length(lab)) {
              # we use the levels
            } else lab <- NULL
          } else {
            catret("Check your data to verify that you can transform",
                   colname,
                   "as factor")
            coldata <- NULL
          }
        } else {
          clevels <- levels
        }
        if (!nlevels == length(clevels)) {
          catret(
            "Check your data to verify that number of categories is correct and match your recoding"
          )
          coldata <- NULL
        }
        if (!is.null(lab)) {
          if (!length(lab) == length(clevels)) {
            catret("Numbers of categories/levels must be equal to number of labels")
            coldata <- NULL
          }
        }
      }
    }
  }
  if (!is.null(coldata)) {
    if (!is.null(lab)) {
      if (reverse) {
        clevels <- rev(clevels)
        lab <- rev(lab)
      }
      coldata <-
        factor(coldata,
               levels = clevels ,
               labels = lab)
    }
    # if (update & is.data.frame(df)) {
    #   df[, colname] <- coldata
    #   # assign(dfname,df,inherits = TRUE )
    #   push.data(dfname, df)
    # 
    
    # This display temporarly removed because difficult to read
    #   cat("** ",colname," **")
    #   cat(" Reordered with labels: ")
    #   catret(levels(coldata))
    
    # }
    # exp <- paste0(substitute(var),"<- coldata")
    # r <- try(evalq(parse(text = exp), envir = df, enclos = .GlobalEnv),TRUE)
    # r
    # df
    coldata
  } else {
    catret(r$var," is not a variable or data.frame column")
  }
  
}



