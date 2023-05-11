#
# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Find it in validDate.RMd


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


#' @title Transforms any character into a valid R Date format
#' 
#' @description validDate process a character or a vector of character (like dataframe column)
#' to verify date format and to transform almost any date formainto standard R date format
#' format can be passed as parameters for worst cases where validDate can't guess the format
#' automaticaly.  
#'
#' @param datevar an (character) array to convert it to a Date array
#' @param format an optional format string to use in place of automatic system, useful for some complex format  
#'
#' @return an (Date) array already converted into the Universal Date Format (UDF) in R (YYYY-mm-dd)
#' @importFrom stats complete.cases
#' @export 
#'
#' @examples
#' datevar <- c("2022/01/15","2022/02/10","1950/01/31","2022/15/15",NA)
#' datevar <- c("20220115","20220210","19500131","111",NA)
#' 
validDate <- function(datevar, format = NULL)  {
  
  # is date character ? if not we skip all
  if (typeof(datevar) == "character") {
    if (!is.null(format)) {
      # format is passed we will use it asis
      cat("Format used passed as parameters :", format)
    }else{
      # format is NULL, we try to guess on 50 first records
      # we take only non NA 
      
        firstrows <- datevar[complete.cases(datevar)]
        # we keep only 50 (or less if smaller)
        firstrows <- firstrows[1:min(50,length(firstrows))]
        # first look for length of datevar 
        lvar <- sapply(firstrows, nchar)
        
        if(any(lvar<6)){
          datevar[which(lvar<6)] <- NA
          firstrows[which(lvar<6)] <- NA
          cat("Record(s)",as.numeric(which(lvar<6)),"with nchar < 6 converted to NA\n") #**modified: any record(s) (<10% of total) transformed to NA
          if(length(which(lvar<6)) >= length(datevar)*0.1){
            cat("Warning: > 10% of datevar converted to NA\n") # **modified: message printed if >10% transformed to NA
          }
        }  
        lvar <- ifelse(lvar<6,NA,lvar)                        # **modified: lvar==0 with lvar<6
        lvarmean <- mean(lvar, na.rm = TRUE) # **suggestion:option b) median 
        
        # then identify the type of separator
        typesep <- c("/","-","\\.","\\s")
        nbsep <- c(1:length(typesep))                                       # **modified: nbsep <- c(1:4) with length(typesep), something more generic
        for (i in 1:length(typesep)) {                                      # **modified: same here, more generic instead of 1:4
          nbsep[i] <- sum(charCount(typesep[i],firstrows))
        }
        typesep <- c("/","-","."," ")
        digit <-  FALSE
        nbmaxsep <-  max(nbsep)
        
        OnlyDigits <- length(grep(pattern = "^[0-9]+$",x = firstrows))>0     # **modified: separator not found/recognized

        if(nbmaxsep==0 & !OnlyDigits){
          cat("Sepatator not recoginized\n")
          format <- NULL
        }else{
          nbvalue <-  length(firstrows)
          if (nbmaxsep > nbvalue  ) {
            indexsep <- which.max(nbsep)
            # needed for stringr::word
            datesep <- ifelse(typesep[indexsep]==".","\\.",typesep[indexsep]) # **modified: to recognized dot
            
          }else{   # too few separator, probably no one
            # if no separator we try to split the string        
            if (! lvarmean > 6) {  # 6 digits without sep
              firstrows <- paste0(substr(firstrows,1,2) , "/" , substr(firstrows,3,4) , "/" , substr(firstrows,5,6) )
              
            } else {                 # **modified: find if year is at the end or at the begining
              # to find a way to test result ... number of NA ? 
              # then try again with year last with firstrows again ? 
              
              StartYear <- min(as.numeric(substr(firstrows,1,4)),na.rm = T)>=1920   # if TRUE = at the begining 
              EndYear <- min(as.numeric(substr(firstrows,5,8)),na.rm = T)>=1920     # if TRUE = at the end 
              if(StartYear){
                firstrows <- paste0(substr(firstrows,1,4) , "/" , substr(firstrows,5,6) , "/" , substr(firstrows,7,8) )
              }else{
                if(EndYear){
                  firstrows <- paste0(substr(firstrows,1,2) , "/" , substr(firstrows,3,4) , "/" , substr(firstrows,5,8) )
                }else{
                  cat("Guessed format: only digits date but year position not found\n")
                }
              }
              
            }
            datesep <- "/"
            digit <- TRUE
          }
          # "." separator doesn't work with word !! must be replaced before splitting in word ...
          
          part <- data.frame(as.numeric(getWord(firstrows,1,pattern=datesep)) )
          part[,2] <- as.numeric(getWord(firstrows,2,pattern=datesep))
          part[,3] <- as.numeric(getWord(firstrows,3,pattern=datesep))
          
          if (digit==TRUE) {datesep <- ""} # **modified: before: (digit==TRUE) {datesep==""}  
          
          # then try to guess the order year first or last then months and days 
          for (i in 1:3) {
            imean <- mean(part[,i],na.rm=TRUE)
            imax <- max(part[,i])
            imin <- min(part[,i])
            if (imean <=12) { datemonths <- i }
            if ((imean > 12) & (imean <= 31))  { datedays <- i }
            if (imean > 31) { 
              dateyears <-  i
              # year format 2 or 4 digit ?
              yearformat <- ifelse(imean>1000,"%Y","%y")
            }
          }
          
          # more tricky : try to identify months in letter format eg November
          
          
          # now we construct format according to finding (see sprptime for formats)
          datesep <- ifelse(datesep=="\\.",".",datesep)    # **modified: if dot, convert again "\\." to "."
          format <- ifelse(dateyears==1,yearformat,ifelse(datedays==1,"%d","%m"))
          format <- paste0(format,datesep,ifelse(datemonths==2,"%m",ifelse(datedays==2,"%d",yearformat) ) )
          format <- paste0(format,datesep,ifelse(datemonths==3,"%m",ifelse(datedays==3,"%d",yearformat) ) )
          
          cat("Format guessed from data : ", format,"\n")
    
        }
    }
    # if format is NULL then something was wrong 
    if (is.null(format)) {
      # is it ok to return NA ?
      return(NA) 
      
    } else { 
      datevar <- as.Date(datevar, format)
      return(datevar)
    }
    
  }else{
    ## datevar is not character ...   any transformation needed ?
    cat("datevar is not character class, must be a character vector to be converted") # **modified: new message if is not character
    
  }
}


# END of SCRIPT  --------------------------------------------------------