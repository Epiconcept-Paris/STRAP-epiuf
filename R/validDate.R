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
# Find it in validDate.Rmd


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------

#' validDate
#' 
#' Description: Validates and converts date formats to standard R date format
#' This function processes a character or a vector of characters to verify the date format 
#' and transform it into a standard R date format. 
#' If the format cannot be automatically determined, you can provide a format string as a parameter.
#'
#' @param datevar A character vector to be converted into a Date vector.
#' @param format An optional format string to use instead of the automatic system. Useful for complex formats.
#' @param dropFuture Logical value. If TRUE and a 2 digit year in the futur (greater than system date), 
#'    it will be placed in the past by substrating one century . Default is TRUE.
#' 
#' @returns A Date object or a vector of Date objects in standard R date format.
#' 
#' @importFrom stats complete.cases
#' @export
#' @author STRAP team \email{strap@epiconcept.fr}
#' @seealso
#' For more details see the link below to access the vignette:
#' \href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
#'
#' @examples
#' validDate("2023-05-25")
#' validDate(c("20221120","20210615","20210303","",NA))
#' validDate(c("05-25-2023", "26/05/2023"), format = "%m-%d-%Y")
#' validDate(datevar = "15/01/60",dropFuture = FALSE)
validDate <- function(datevar, format = NULL,dropFuture=TRUE){
  
  # is date character ? if not we skip all
  if (typeof(datevar) == "character") {
    if (!is.null(format)) {
      # format is passed we will use it asis
      catret("Format used passed as parameters :", format)
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
          cat("Record(s)",as.numeric(which(lvar<6)),"with nchar < 6 converted to NA\n") #**modified: any record(s) (<10% of total) transformed to NA
          if(length(which(lvar<6)) >= length(datevar)*0.1){
            cat("Warning: > 10% of datevar converted to NA\n") # **modified: message printed if >10% transformed to NA
          }
          firstrows[which(lvar<6)] <- NA
          firstrows <- firstrows[complete.cases(firstrows)]
          lvar <- sapply(firstrows, nchar)
        }  
        lvar <- na.omit(lvar)
        suppressWarnings(lvarmean <- mean(lvar, na.rm = TRUE)) # **suggestion:option b) median 
        if( !is.numeric(lvarmean)) lvarmean <- 1
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
          
          part <- data.frame(as.numeric(getWord(firstrows,1,pattern=datesep)))
          part[,2] <- as.numeric(getWord(firstrows,2,pattern=datesep))
          part[,3] <- as.numeric(getWord(firstrows,3,pattern=datesep))
          
          if (digit==TRUE) {datesep <- ""} # **modified: before: (digit==TRUE) {datesep==""}  
          datedays <- datemonths <- dateyears <- 0
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
          if(datedays*datemonths*dateyears==0) {
            format <- NULL
            cat("Not enough values to guess format\n")
          }
          else {  
            # more tricky : try to identify months in letter format eg November
            
            # now we construct format according to finding (see sprptime for formats)
            datesep <- ifelse(datesep=="\\.",".",datesep)    # **modified: if dot, convert again "\\." to "."
            format <- ifelse(dateyears==1,yearformat,ifelse(datedays==1,"%d","%m"))
            format <- paste0(format,datesep,ifelse(datemonths==2,"%m",ifelse(datedays==2,"%d",yearformat) ) )
            format <- paste0(format,datesep,ifelse(datemonths==3,"%m",ifelse(datedays==3,"%d",yearformat) ) )
            
            cat("Format guessed from data : ", format,"\n")
          }
       }
    }
    # if format is NULL then something was wrong 
    if (is.null(format)) {
      # is it ok to return NA ?
      return(NA) 
      
    } else { 
      datevar <- as.Date(datevar, format)
      # check for date after system date (to change century)
      if(dropFuture){ # if the user ask for drop future dates
        indexfut <- which(datevar>Sys.Date()) 
        if(length(indexfut) > 0){
          # substract one century (in days)
          datevar[indexfut] <- datevar[indexfut]-(365.25)*100
        }
      }
      return(datevar)
    }
    
  }else{
    ## datevar is not character ...   any transformation needed ?
    cat("datevar is not character class, must be a character vector to be converted") # **modified: new message if is not character
    
  }
}


# END of SCRIPT  --------------------------------------------------------