#
# Project Name : epiuf 
# Script Name  : datasourcetools.R
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Collection of function to manage data source files. Maintain a list of original sources files
# Allows to retrieve new files, and to make summary report
# Can keep trace of dates for each stage of the process of importing source files
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------


# END of SCRIPT  --------------------------------------------------------

# epiSourceFiles environement used to manage global values
episourcefiles_env <- new.env(parent = emptyenv())

episourcefiles_env$data <- NULL
episourcefiles_env$datafilename <- NULL
episourcefiles_env$rootdir <- NULL


asDateTime <- function(time) {
  return(as.character(as.POSIXlt(time,format = "%Y-%m-%d %H:%M:%S")))
}

#' openSourceList
#'
#' @param filename Name of the excel file containing sources information
#'                 If NULL then default to "SOURCES/datasources.xls" 
#'                 if "" then current name
#'                 (this is used to reload current datasource list) 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{ openSourceList("datasources.xls")}
openSourceList <-  function(filename=pathToFile("SOURCES","datasources.xls")) {
  if (filename=="" & ! is.null(episourcefiles_env$datafilename)) {
     filename <- episourcefiles_env$datafilename 
  }
  if (file.exists(filename)) {
    episourcefiles_env$data <- readData(filename)
    # we need to update structure if needed
    episourcefiles_env$data <- updateDataset(episourcefiles_env$data,getDataLine())
  } else {   # dataSourceList doesn't exist we have to create it
    red("Datasourcelist ",filename," not found. Empty list created")
    episourcefiles_env$data <- getDataLine()
  }
  episourcefiles_env$datafilename <- filename
  episourcefiles_env$rootdir <- dirname(filename)
}

#' Title
#'
#' @param filename Name of the Excel file containing source information
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{ createSourceList("datasources.xls") }
createSourceList <- function(filename) {
  episourcefiles_env$data <- getDataLine()
  episourcefiles_env$datafilename <- filename
  episourcefiles_env$rootdir <- dirname(filename)
}


# we need to add the current status ! 
getDataLine  <- function() {
  OneDataLine <- data.frame(FileName=as.character(),              # name of the source
                            FilePath=as.character(),              # original path of the source 
                            FileDate=asDateTime(character()),     # date of source file 
                            Country=as.character(),               # country code
                            Period=as.character(),                # period 
                            SourceType=as.character(),            # type od data (COV, FLU, LAB...)
                            SourceStatus=as.character(),          # status of source (NEW, IMPORTED,CHECKED etc)
                            CurrentStatus=as.logical(),           # TRUE if current status
                            StatusDate=asDateTime(character()),   # date of this status
                            nbRecords=as.integer(),               # nb records (updated after import or check)
                            stringsAsFactors=FALSE
  )
  return(OneDataLine)
}


sortSourceList <- function() {
  if (length(episourcefiles_env$data) >0 ) {  
    episourcefiles_env$data <- episourcefiles_env$data[rev(order(episourcefiles_env$data$FileName,
                                                                 episourcefiles_env$data$FileDate) 
    ), ]
  }
}


#' searchNewFile 
#' 
#' Part of datasources management
#' Look into a folder (usually a country folder) to find new or updated datasources
#' If such datasources are found, they are imported in the datasourcelist with NEW or UPDATED status and current date
#' DatasourceList contain one line for each new event   
#'
#' @param path Path to serach for (relative to the path where is the datasources file)
#' @param country Country code to attach to the file information
#' @param pattern Optional pattern to restrict file list
#'
#' @return List of new files (file names)
#' @export
#'
#' @examples
#' \dontrun{ searchNewFiles("sourcesdir","FR") }
searchNewFiles <- function(path,country="",pattern=NULL) {
  
  if (is.null(getSourceList())) {
    warning("Open source list before using source file functions\n")
  } 
  if (!is.null(pattern))  pattern <- glob2rx(pattern)
  # fullpath is rootdir for sources + path parameter
  fullpath <- file.path(episourcefiles_env$rootdir,path)
  # Get files names in the folder set by the path 
  filesnames <- list.files(fullpath,pattern)
  # we need file full name to get datetime information
  filesfullnames <- list.files(fullpath, pattern, full.names = TRUE)
  # we apply rootdir to obtain path from sources to make datasources.xls dir independant
  filespaths <- lapply(as.vector(filesnames),function(x) file.path(path,x))
  filespaths <- unlist(filespaths)
  # added will contain list of added files for immediate use 
  # these files are also added to the main sourcelist
  added <- list()
  if ( length(filesnames)>0 ) {
    # For each file name
    for (i in 1:length(filesnames)) {
      
      # Verify if file extension is not .txt : do we keep this control ?
      # I also add ".rdata" because the readData fn doesnt read this type of data yet
      if(fileExt(filesnames[i]) == "txt" | fileExt(filesnames[i]) == "rdata"){
        catret("File", filesnames[i], "has been ignored.")
      } else {  # start valid extension
        
        # Empty record to save the information
        res <- getDataLine()
        res[1,] <- NA
        res$FileName <- filesnames[i]
        res$FilePath <- filespaths[i]  # the path from sources
        res$FileDate <- asDateTime(file.mtime(filesfullnames[i]))  
        # Country : Get the name of the last folder (ie. the country) where the file is located
        # res$Country <- sub(".*/", "", sub(paste0("/",filesnames[i], ".*"), "", filespaths[i]))
        res$Country <- country
        
        # Period : could be in the file name (but not safe) or in a variable or have a dialog with the user
        res$Period <- ""
        res$SourceType <- ""
        res$SourceStatus <- NA  
        res$StatusDate <- asDateTime(Sys.time())
        res$CurrentStatus <- TRUE
  
        # Verify if the file already exists in SourceList 
        if ( ! (res$FileName %in% episourcefiles_env$data[,"FileName"])) {
          # We will add it 
          res$SourceStatus <- "NEW"
        } else { 
          # is the file updated ? (Already exist same filename with older date)
          # posix date precision may be sligthy different and 2 posixlt date can not be compared
          # then we use the character representation!
          older <- getSourceList()
          older <- subset(older, (older$SourceStatus == "NEW" & 
                                    older$FileName == res$FileName) )
          # if more than on we sort in reverse order 
          if (nrow(older)>1) {  
            older <- older[rev(order(older$FileDate)),]
            # we keep only the last one
            older <- older[1,]
          }
          if ( any(older$FileDate < res$FileDate) ) {
            res$SourceStatus <- "UPDATED"
          }
        } 
        
        # file is either NEW or UPDATED , we add it if user confirm
        if (! is.na(res$SourceStatus)) {
          # if ( confirm(paste("File", filesnames[i], "is not in SourceList. Do you want to add it ?")) ) {
          if(TRUE) {  
            # we remove read of data which may took long time for big set
            # df <- readData(filespaths[i])
            
            # Add the information to SourceList (add a row)
            episourcefiles_env$data <- rbind(episourcefiles_env$data, res)
            # Add the name to a list for summary output 
            added <- append(added,res$FileName)
            
            # Print message
            catret("File", filesnames[i], " added to SourceList.")
          }
        }  
          
      } # valid extension
        
    } # for loop
    
    # At the end, we keep the sourcelist sorted on FileNameFileDate
    sortSourceList()

  }  
  return(added)
}


#' getFileList  Return a list of files for specific status 
#'
#' @param country   The country code 
#' @param status    The current status searched for  (where CurrentStatus is TRUE )
#' @param current   TRUE by default, return only the current status (one line) 
#' @param periodstart Optional
#' @param periodend Optional
#' @param sourcetype Optional
#'
#' @return  a dataset containing files informations 
#' @export
#'
#' @examples
#' \dontrun{  getFileList("FR","IMPORTED")} 
getFileList<- function(country=NULL, status=NULL,current = TRUE, periodstart=NULL, periodend=NULL, sourcetype =NULL) {

  # we start with the country
  res <- getSourceList()
  res <- subset(res, res$Country == country)
  # sorted from most recent status
  # iorder <- order(rev(res$StatusDate))
  # res <- res[iorder,]
  res <- sortBy(res,"StatusDate",decreasing=TRUE)
  # we keep only the current ststus
  if (!is.null(current)) {
    res <- subset(res, res$CurrentStatus == current )
  }
  
  
  # we select the sourcetype if not NULL 
  if (!is.null(sourcetype)) {
    res <- subset(res, res$SourceType %in% sourcetype )
  }
  
  # then we look for status
  if (!is.null(status)) {
    res <- subset(res, res$SourceStatus %in% status )
  }
  # we keep only the current ststus
  
  return(res)
}

#' getSourceList
#'
#' @return the dataset containing all source information
#' @export
#'
#' @examples
#' \dontrun{ getSourceList() }
getSourceList <- function() {
  # if (is.null(episourcefiles_env$data)) createSourceList()
  return(episourcefiles_env$data)
}

#' setSourceList
#'
#' @param datasourcedf a dataframe containing sources information
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{ setSourceList(dfsourcelist) }
setSourceList  <- function(datasourcedf) {
  episourcefiles_env$data <- datasourcedf
}

#' exportSourceList
#'
#' @param filename A filename where to save the source list
#'
#' @return nothing
#' @export
#'
#' @examples
#' # nothing to show 
exportSourceList <- function(filename){
  
}

#' saveSourceList
#'
#' @param filename Name of the file containing sources informations
#'
#' @return nothing
#' @export
#' 
#' @importFrom xlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' saveSourceList("datasourcelist.xls")
#' }
saveSourceList <- function(filename=NULL) {
  # checks to be added
  if (is.null(filename)) {
    filename <- episourcefiles_env$datafilename
      if (is.null(filename)) {
        filename <- pathToFile("SOURCES","datasources.xls")
      }
  }
   ds <- getSourceList()
   if (is.null(ds)) createSourceList()
   ds <- getSourceList() 
   if (nrow(ds)==0) {
      ds[1,] <- NA
   }
   xlsx::write.xlsx(ds,file=filename,row.names=FALSE)
}

#' updateSourceData
#'
#' @param sourcefilename Name of the source file to update with new informations
#' @param status : IMPORTED/CHECKED/VALIDATED/POOLED 
#' @param sourcetype optional source type 
#' @param date Date to be used as timestamp 
#' @param nbrecords Optional number of records to add to the file information 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{ 
#' # update status when file is imported 
#' updateSourceData(filename,status="IMPORTED") 
#' }
updateSourceData <- function(sourcefilename,status=c("NEW","UPDATED","IMPORTED","CHECKED",
                                                     "VALIDATED","POOLED")
                             ,sourcetype=NULL,date=Sys.time(), nbrecords=NULL ) {
   # verify that datasourcefile is loaded
   date <- asDateTime(date)
   ds <- getSourceList()
   if (!is.null(ds)) {
     # verify that source exist
     dscountry <- subset(ds,ds$FileName==sourcefilename)
     if (!is.null(sourcetype)) dscountry <- subset(dscountry,dscountry$SourceType==sourcetype)
     if (nrow(dscountry)>0) {  
       dslast <- as.data.frame(subset(dscountry,dscountry$CurrentStatus==TRUE))
       if (nrow(dslast)>0 ) {
          # current is no longer the current !
          ds[ds$FileName==sourcefilename & ds$CurrentStatus==TRUE,"CurrentStatus"] <- FALSE
       }
       # we add copy of current one to the list with the new status and new date
       dslast$SourceStatus <- status
       dslast$StatusDate <- asDateTime(date)
       if (! is.null(nbrecords)) dslast$nbRecords <- nbrecords
       row.names(dslast) <- as.character(nrow(ds)+1) 
       ds <- rbind(ds,dslast)
       # update source
       setSourceList(ds)
     }
   }
}

#' sourceSummary
#'
#' Produce a table of status by country/period
#'
#' @param country For specific conutry table
#' @param startdate First date 
#' @param enddate   Last date 
#'
#' @return a dataframe ready to print
#' @export
#'
#' @examples
#' \dontrun{ sourceSummary("FR")}
sourceSummary <- function(country, startdate = NULL, enddate = NULL) {
  # Summary of all files for one or sevral countries
  res <- episourcefiles_env$data
  if(is.null(startdate) & is.null(enddate)){
    res <- subset(res, res$Country %in% country)
  }
  # Summary of all files for one country or several countries and from a date
  if(!is.null(startdate) & is.null(enddate)){
    res <- subset(res, res$Country %in% country & res$FileDate >= startdate)
  }
  # Summary of all files for one country or several countries and for a period given (from startdate up to enddate)
  if(!is.null(startdate) & !is.null(enddate)){
    res <- subset(res, res$Country %in% country & (res$FileDate >= startdate & res$FileDate < enddate))
  }
  return(res)
}

# function <- rebuild/construct ?

