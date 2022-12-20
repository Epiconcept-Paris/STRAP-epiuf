# Open Excel File
# report <- xlsx::loadWorkbook(file = pathToFile("EXCEL","Tables/Tables_Modify_R.xlsx"))
# sheets <- getSheets(report)
# sheet_edit <- sheets[['T2']]
# rows_edit <- getRows(sheet_edit)
# cells <- getCells(rows_edit)



# epixlsx environment used to manage global values
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
  return(epixlsx_env$report)
}

#' fillCells
#'
#' @param onesheet A sheet object from xlsx package
#' @param line  The line where to paste value 
#' @param col   The col where to paste value
#' @param ...   List of N values to paste in col "col" to col+N 
#'              if contain a data.frame, the dataframe is pasted at the position 
#' @param names if TRUE column and row names of the dataframe are displayed (overwrite colnames and rownames)
#' @param colnames if TRUE column names of the dataframe are displayed 
#' @param rownames if TRUE row names of the dataframe are displayed             
#' @param style An optional style created with createXlsxStyle   
#' @param styleRowsIndex Numeric vector of the rown index where to apply the style
#' @param styleColsIndex Numeric vector of the column index where to apply the style
#' @param wb An optional wb if not already opened   
#' @return  nothing
#' @export
#' @importFrom openxlsx  writeData col2int 
#'
#' @examples
#' 
#' # Creating an empty workbook
#' wb <- openxlsx::createWorkbook()
#' sheetname <- "First Sheet"
#' openxlsx::addWorksheet(wb, sheetName = sheetname)
#' num <- 3
#' denum <- 10
#' fillCells(sheetname, 1, 1, num, denum , wb = wb)
#' mat <- data.frame(Id = 1:3 , Vaccs = c("1", "3", "6"))
#' fillCells(sheetname, 3, 1, mat, wb = wb)
#' 
#' # Writing a data frame in the workbook using a style to apply for a set of rows and columns
#' alertStyle <- createXlsxStyle(textDecoration = "Bold", fontColour = "red")
#' fillCells(wb = wb, 
#'           onesheet = sheetname, 
#'           line = 7, 
#'           col = 2, 
#'           iris, 
#'           style = alertStyle, 
#'           styleRowsIndex = which(iris$Sepal.Length<5),
#'           styleColsIndex = 1:4)
#'           
#' # Saving the Excelfile
#' \dontrun{
#' saveXlsx(filename = "test.xlsx", wb = wb)
#' }
#'
fillCells <- function(onesheet,
                      line,
                      col, 
                      ... , # All following arguments are optional but should be named 
                      names = FALSE, 
                      colnames = FALSE, 
                      rownames = FALSE, 
                      style = NULL, 
                      styleRowsIndex = NULL,  
                      styleColsIndex = NULL,
                      wb = NULL) {
  
  # If col is provided as a character string (i.e., col = "A") 
  # then will be converted as column index 
  if (is.character(col)){
    col <- openxlsx::col2int(col)
  }
  
  # Default workbook being in package's env
  if (is.null(wb)) wb <- epixlsx_env$report #LM: why is it in the loop and not outside the loop?
  
  # Global argument 'names' setting both colnames and rownames 
  if (names == TRUE) {
    colnames = TRUE
    rownames = TRUE 
  }
  
  # Warning the end user if style is missing
  if(is.null(style) & (!is.null(styleRowsIndex) | !is.null(styleColsIndex))) {
    warning("'styleRowsIndex'/'styleColsIndex' are provided but 'style' argument is missing. No style willbe applied")
  }
  
  # Fetching all objects to print in xlsx
  listval <- list( ...) 
  for (i in 1:length(listval)) { 
    value <- ...elt(i)   
    
    # Making sure any numeric vector is finite or replace with empty string
    if (is.numeric(value)) {
      value <- ifelse(is.finite(value), value, "")
    }
    
    # Write in workbook
    openxlsx::writeData(wb = wb, 
                        sheet = onesheet, 
                        x = value, 
                        startCol = col, 
                        startRow = line,
                        colNames = colnames, 
                        rowNames = rownames)
    
    # Adding style if required
    if (!is.null(style)) {
      
      # Checking if condition to apply the style is valid or not
      if(!is.null(styleRowsIndex) | !is.null(styleColsIndex)) { 
        
        # For list or array
        
        if(inherits(value, c("list", "array"))) {
          warning("Class ", class(value), " is currently not supported for style in fillCell")
        } else if(inherits(value, "data.frame")) {
          
          # For data frames
          
          # Checking that style condition is TRUE/FALSE
          if (!(typeof(styleRowsIndex) %in% c("integer", "double", "NULL")) | 
              !(typeof(styleColsIndex) %in% c("integer", "double", "NULL"))) {
            warning("Type not supported for 'styleRowsIndex' / 'styleColsIndex' arguments")
          }
          
          # Checking if one of the condition is NULL and replace it with dimension of the object
          if(is.null(styleRowsIndex)) {styleRowsIndex <- 1:nrow(value)}
          if(is.null(styleColsIndex)) {styleColsIndex <- 1:ncol(value)}
            
          # Adding style at the proper location
          openxlsx::addStyle(wb = wb, 
                             sheet = onesheet, 
                             style = style, 
                             rows = line + styleRowsIndex - 1, 
                             cols = col + styleColsIndex - 1,
                             gridExpand = TRUE)
          
        } else {
          
          # For atomic variables or vector
          
          # Checking that style condition is TRUE/FALSE
          if (!(typeof(styleRowsIndex) %in% c("integer", "double"))) {
            warning("Type not supported for 'styleRowsIndex' / 'styleColsIndex' arguments")
          }
          
          # Adding style at the proper location
          openxlsx::addStyle(wb = wb, 
                             sheet = onesheet, 
                             style = style, 
                             rows = line + styleRowsIndex - 1, 
                             cols = col)
        }  # end List, Array, Data frame, Vector, Variable 
        
      } else { # If styleIndex is NULL, then style is expanded to the size of the object
        
        # List or array
        
        if(inherits(value, c("list", "array"))) {
          warning("Class ", class(value), " is currently not supported for style in fillCell")
        } else if(inherits(value, "data.frame")) {
          
          # Data frames
          openxlsx::addStyle(wb = wb, 
                             sheet = onesheet, 
                             style = style, 
                             rows = line + 1:nrow(value) - 1, 
                             cols = col + 1:ncol(value) - 1,
                             gridExpand = TRUE)
          
        } else {
          
          # Vectors or variables
          openxlsx::addStyle(wb = wb, 
                             sheet = onesheet, 
                             style = style, 
                             rows = line + 1:length(value) - 1, 
                             cols = col,
                             gridExpand = TRUE)
          
        } # end List, Array, Data frame, Vector, Variable
      } # end no styleIndex
    } # end Style
    
    col <- col +1
    
  }  # end loop i
}
# # Previous version below
# fillCells <- function(onesheet,line,col, ... , names = FALSE, colnames=FALSE, 
#                       rownames=FALSE, style = NULL, stylecondition = NULL,  wb = NULL) {
#   
#   # If col is provided as a character string (i.e., col = "A") then converted as column index 
#   if (is.character(col)){
#     col <- openxlsx::col2int(col)
#   }
#   
#   if (names == TRUE) {
#     colnames = TRUE
#     rownames = TRUE 
#   }
#   
#    listval <- list( ...)
#    for (i in 1:length(listval)) {
#      value <- ...elt(i)   
#      if (is.numeric(value)) {
#         value <- ifelse(is.finite(value),value,"")
#      }
#      if (is.null(wb)) wb <- epixlsx_env$report # LM: why is it in the loop and not outside the loop?
#      openxlsx::writeData(wb, onesheet, x = value, startCol = col, startRow = line,
#                colNames = colnames, rowNames = rownames)
#      
#      # Adding style if required
#      if (!is.null(style)) {
#          openxlsx::addStyle(wb, onesheet, style, line, col)
#      }   
#      col <- col +1
#       
#   }  # end loop i
# }


#' formatCells
#'
#' @param onesheet A sheet object from xlsx package
#' @param line  The line where to paste value 
#' @param col   The col where to paste value
#' @param style A style created by createXlsxStyle
#' @param wb  An optional wb if not already opened
#'
#' @return nothing
#' @export
#'
formatCells <- function(onesheet, line, col , style = NULL, wb = NULL)
{
  
  if (is.character(col)){
    col <- openxlsx::col2int(col)
  }
  if (is.null(wb)) wb <- epixlsx_env$report
    if (! is.null(style)) {
    openxlsx::addStyle(wb, onesheet, style, line, col) # LM: could we add ... in order to possibly benefit from additional arguments like createStyle?
  }   
  
}  
  

#' createXlsxStyle
#'
#' @param ...   Parameters for createStyle 
#'
#' @return a Style to be used in XLSX
#' @export
#'
createXlsxStyle <- function(...) {
  openxlsx::createStyle(...)
}





#' fillimage
#'
#' @param onesheet A sheet object from xlsx package
#' @param image The filename of a previously saved image 
#' @param line The line where to paste the image
#' @param col The col where to paste the image
#' @param wide Size of the image 
#' @param high Size of the image
#' @param unit For wide and high, default is "in" (Inch) 
#' @param spec.dpi 300 by default
#' @param wb  An optional wb if not already opened
#'  
#' @return Nothing
#'  
#' @export
#'
#' @examples 
#' \dontrun{ fillimage(onesheet = cells,image = graphname,line=2, col = 2, wb=template) }
#' 
fillimage <- function(onesheet,image,line,col, wide=7, high=4 , unit = "in", spec.dpi=300, wb = NULL) {
  
  if (is.character(col)){
    col <- openxlsx::col2int(col)
  }
  
  if (is.null(wb)) wb <- epixlsx_env$report
  
  openxlsx::insertImage( wb , onesheet, file = image, width = wide, height=high
                         , startCol = col, startRow = line, units=unit, dpi=spec.dpi)
  
  
}


#' openSheet
#'
#' @param sheetname The name of the sheet to load 
#' @param wb       A optional workbook created or opened by xlsx.
#'                 If not specified, the last workbook loaded with openXlsx will be used
#'
#' @return  Character string of the name of the sheet
#' @importFrom openxlsx createWorkbook    
#' @export
#'
#' @examples
#'  wb <- openxlsx::createWorkbook()
#'  openxlsx::addWorksheet(wb, sheetName = "First Sheet")
#'  openSheet("First Sheet",wb)
#' 
openSheet <- function(sheetname, wb = NULL)  {
  if (! is.null(wb)) {
    report <- wb
  } else { 
    report <- epixlsx_env$report
    if (is.null(epixlsx_env$report)) {
       cat("Excel file must be loaded before opening a sheet")
    } 
  }
  
  # sheets <- openxlsx::sheets(report)  # GD: names is not exported ??
  sheets <- names(report) # LM: Replaced openxlsx::sheets with base::names as this function is deprecated  
  if ( !(sheetname %in% sheets)) {
     warning("Sheet ", sheetname, " doesn't exist in workbook")
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
#' # Creating an empty workbook
#' wb <- openxlsx::createWorkbook()
#' sheetname <- "First Sheet"
#' openxlsx::addWorksheet(wb, sheetName = sheetname)
#' 
#' # Writing a data frame in the workbook 
#' fillCells(wb = wb, 
#'           onesheet = sheetname, 
#'           line = 1, 
#'           col = 1, 
#'           iris)
#'           
#' # Saving the Excel file
#' \dontrun{
#' saveXlsx(filename = "test.xlsx", wb = wb)
#' }
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
