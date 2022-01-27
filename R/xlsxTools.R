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
#' @param cells A cells object from xlsx package
#' @param line  The line where to paste value 
#' @param col   The col where to paste value
#' @param ...   List of N values to paste in col "col" to col+N 
#'
#' @return  nothing
#' @export
#' @importFrom xlsx setCellValue
#'
#' @examples
#' wb <- xlsx::createWorkbook()
#' sheet <- xlsx::createSheet(wb,"S1")
#' rows <- xlsx::createRow(sheet, rowIndex = 1:5)
#' sheetTitle <-xlsx::createCell(rows, colIndex=1:3)
#' cells <- openSheet("S1",wb)
#' 
#' fillCells(cells,1,1, "Total :",100  )
#' num <-  40
#' denum <- 80
#' fillCells(cells,1,1, num,num/denum  )
#' 
#' 
fillCells <- function(cells,line,col, ...) {
  listval <- eval(substitute(alist(...)))
  for (i in 1:length(listval)) {
    cell<- paste0(line,".",col)
    value <- ifelse(is.finite(eval(listval[[i]])),eval(listval[[i]]),"")
    xlsx::setCellValue(cells[[cell]],value  ) 
    col <- col +1 
  }
}


#' openSheet
#'
#' @param sheetname The name of hte sheet to load 
#' @param wb       A optional workbook created or opened by xlsx
#'                 If not specified, the last workbook loaded with loadXlsx will be used
#'
#' @return  A cells object containing an Excel sheet
#' @importFrom xlsx createWorkbook getSheets getRows getCells 
#' @export
#'
#' @examples
#'  wb <- xlsx::createWorkbook()
#'  sheet <- xlsx::createSheet(wb,"S1")
#'  rows <- xlsx::createRow(sheet, rowIndex = 1:5)
#'  sheetTitle <-xlsx::createCell(rows, colIndex=1)
#' 
#'  cells <- openSheet("S1",wb)
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
  sheets <- getSheets(report)
  sheet_edit <- sheets[[sheetname]]
  rows_edit <- getRows(sheet_edit)
  cells <- getCells(rows_edit)
}


#' loadXlsx
#'
#' @param filename Name of the Excel file
#'
#' @return The wb (which is also saved as internal variable)
#' @export
#' @importFrom xlsx loadWorkbook
#'
#' @examples
#' cat("to be done")
#' 
openXlsx <- function(filename="") {
  epixlsx_env$report <- xlsx::loadWorkbook(file = filename)
  epixlsx_env$reportFilename <- filename
  cat(filename, "workbook loaded in memory")
  invisible(epixlsx_env$report)
}


#' Title
#'
#' @param wb A optional workbook created or opened by xlsx
#'                 If not specified, the last workbook loaded with loadXlsx will be used
#' @param filename An optional filename (if empty the current filename will be used )
#'
#' @return nothing
#' @export
#' @importFrom xlsx saveWorkbook
#'
#' @examples
#' 
#' cat("to be done")
#' 
saveXlsx <- function(wb=NULL,filename="")  {
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
    xlsx::saveWorkbook(report, filename)
    cat("Workbook saved as :",filename)
  }
}
