# Open Excel File
# report <- xlsx::loadWorkbook(file = pathToFile("EXCEL","Tables/Tables_Modify_R.xlsx"))
# sheets <- getSheets(report)
# sheet_edit <- sheets[['T2']]
# rows_edit <- getRows(sheet_edit)
# cells <- getCells(rows_edit)



# epixlsx envirronement used to manage global values
epixlsx_env <- new.env(parent = emptyenv())

epixlsx_env$report <- NULL
epixlsx_env$reportFilename <- NULL


#' Title
#'
#' @return The current workbook in memory
#' @export
#'
#' @examples
#' getWorkbook()
#' 
getWorkbook <- function() {
  return(  epixlsx_env$report )
}

#' fillCells
#'
#' @param onesheet A sheet object from xlsx package
#' @param line  The line where to paste value 
#' @param col   The col where to paste value
#' @param ...   List of N values to paste in col "col" to col+N 
#'              if contain a data.frame, the dataframe is pasted at the position 
#' @param names if TRUE names of the dataframe are inserted with the table content              
#' @param wb An optional wb if not already opened   
#' @return  nothing
#' @export
#' @importFrom openxlsx  writeData col2int addWorksheet
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' sheetname <- "First Sheet"
#' openxlsx::addWorksheet(wb, sheetName = sheetname)
#' num <- 3
#' denum <- 10
#' fillCells(sheetname,1,1, num,denum , wb=wb )
#' mat <- data.frame(Id = 1:3 , Vaccs = c("1", "3", "6"))
#' fillCells(sheetname,1,1, mat ,wb=wb )
#' 
fillCells <- function(onesheet,line,col, ... , names=FALSE, wb = NULL) {
  
  if (is.character(col)){
    col <- col2int(col)
  }
   listval <- list( ...)
   for (i in 1:length(listval)) {
     value <- ...elt(i)   
     if (is.numeric(value)) {
        value <- ifelse(is.finite(value),value,"")
     }
     if (is.null(wb)) wb <- epixlsx_env$report
     writeData( wb , onesheet, x = value, startCol = col, startRow = line,
               colNames = names, rowNames = names)
     col <- col +1
      
  }  # end loop i
}


#' openSheet
#'
#' @param sheetname The name of hte sheet to load 
#' @param wb       A optional workbook created or opened by xlsx
#'                 If not specified, the last workbook loaded with loadXlsx will be used
#'
#' @return  A cells object containing an Excel sheet
#' @importFrom openxlsx createWorkbook addWorksheet sheets  
#' @export
#'
#' @examples
#'  wb <- openxlsx::createWorkbook()
#'  openxlsx::addWorksheet(wb, sheetName = "First Sheet")
#'  openSheet("First Sheet",wb)
#' 
openSheet <- function(sheetname,wb=NULL)  {
  if (! is.null(wb)) {
    report <- wb
  } else { 
    report <- epixlsx_env$report
    if (is.null(epixlsx_env$report)) {
       cat("Excel file must be loaded before opening a sheet")
    } 
  }
  
  sheets <- openxlsx::sheets(report)  # names is not exported ??
  if ( !(sheetname %in% sheets)) {
     warning("Sheet",sheetname, "doesn't exist in workbook")
     sheetname <- NULL 
  } 
  sheetname
}


#' loadXlsx
#'
#' @param filename Name of the Excel file
#'
#' @return The wb (which is also saved as internal variable)
#' @export
#' @importFrom openxlsx loadWorkbook
#'
#' @examples
#' cat("to be done")
#' 
openXlsx <- function(filename="") {
  wb <- openxlsx::loadWorkbook(filename)
  epixlsx_env$report <- wb
  epixlsx_env$reportFilename <- filename
  cat(filename, "workbook loaded in memory")
  invisible(epixlsx_env$report)
}


#' saveXlsx 
#'        This function save the current workbook as filename  
#'
#' @param wb A optional workbook created or opened by xlsx
#'                 If not specified, the last workbook loaded with loadXlsx will be used
#' @param filename An optional filename (if empty the current filename will be used )
#'
#' @return nothing
#' @export
#' @importFrom openxlsx saveWorkbook
#'
#' @examples
#' 
#' cat("to be done")
#' 
saveXlsx <- function(filename="",wb=NULL)  {
  if (! is.null(wb)) {
     report <- wb
  } 
  else { 
    report <- epixlsx_env$report
  }
  if (is.null(report)) {
       cat("No Excel file loaded")
  }
  else {
    if (filename=="") {
      filename <- epixlsx_env$reportFilename
    }
    openxlsx::saveWorkbook(report, filename, overwrite = TRUE)  ## save to working directory
    cat("Workbook saved as :",filename)
  }
}



# openxlsx ----------------------------------------------------------------
# 
# library(openxlsx)
# boldHeader <- createStyle(textDecoration = 'bold') # Makes first row bold
# wb <- loadWorkbook('Tables.xlsx')
# 
# if (!('Supplemental Table 1' %in% names(wb))) addWorksheet(wb, 'Supplemental Table 1')
# writeData(wb, 'Supplemental Table 1', results, headerStyle = boldHeader)
# setColWidths(wb, 'Supplemental Table 1', cols = 1:ncol(results), widths = 'auto')
# saveWorkbook(wb, 'Tables.xlsx', overwrite = T)
# 
# library(openxlsx)
# write.xlsx(iris, file = "writeXLSX1.xlsx")
# 
# # write one table per sheet  name = table
# l <- list(IRIS = iris, MTCARS = mtcars)
# # write.xlsx return the workbook 
# wb <-  write.xlsx(l, file = "writeXLSX2.xlsx")
# # then we can modify it
# setColWidths(wb, sheet = 1, cols = 1:5, widths = 20)
# # and we can save it 
# saveWorkbook(wb, "writeXLSX2.xlsx", overwrite = TRUE)
# 
# # we can create a workbook 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "First Sheet")
# # write data to sheet 1
# writeData(wb, 1, x = "Iris dataset group means", startCol = "AA", startRow = 2)
# writeDataTable(wb, sheet = 1, x = mtcars, colNames = TRUE, rowNames = TRUE, tableStyle = "TableStyleLight9")
# # writeData to sheet 2
# addWorksheet(wb, sheetName = "Second Sheet")
# writeData(wb, 2, x = "Iris dataset group means", startCol = 2, startRow = 2)
# means <- aggregate(x = iris[, -5], by = list(iris$Species), FUN = mean)
# writeData(wb, 2, x = means, startCol = "B", startRow = 3)
# writeData(wb, 2, x = c(1,2,3), startCol = "B", startRow = 12)
# saveWorkbook(wb, "basics.xlsx", overwrite = TRUE)  ## save to working directory
# 
# # we can load a workbook
# wb <- loadWorkbook(system.file("extdata", "readTest.xlsx", package = "openxlsx"))
# # or just read excel into a data.frame
# df3 <- read.xlsx(wb, sheet = 2, colNames = TRUE)
# 
# # or just some data 
# wb <- loadWorkbook(system.file("extdata", "readTest.xlsx", package = "openxlsx"))
# df3 <- read.xlsx(wb,
#                  sheet = 2, skipEmptyRows = FALSE,
#                  cols = c(1, 4), rows = c(1, 3, 4)
# )
# 
# wb <- loadWorkbook("datasources.xlsx")
# 
