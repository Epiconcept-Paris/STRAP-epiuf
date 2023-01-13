# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description -------------------------------------------------------------
#'

# Changes Log -------------------------------------------------------------
#' 

# START of SCRIPT  --------------------------------------------------------

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
  # WriteData() will automatically convert it, but it is fine to double check! :)
  # (e.g. which(LETTERS == "C"))
  if (is.character(col)){
    col <- openxlsx::col2int(col)
  }
  
  # Default workbook being in package's env
  if (is.null(wb)) wb <- epixlsx_env$report
  
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
          # Note: vectors are printed in 1 column, therefore only rows can be styled
          if (!(typeof(styleRowsIndex) %in% c("integer", "double"))) {
            warning("Type not supported for 'styleRowsIndex' arguments")
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



#' formatCells
#'
#' @param onesheet A sheet object from xlsx package
#' @param line  The line where to paste value 
#' @param col   The col where to paste value
#' @param style A style created by createXlsxStyle
#' @param wb  An optional wb if not already opened
#' @param ... any additional style accedpted by openxlsx::addStyle()
#'
#' @return nothing
#' @export
#'
formatCells <- function(onesheet, line, col , style = NULL, wb = NULL, ...)
{
  
  if (is.character(col)){
    col <- openxlsx::col2int(col)
  }
  if (is.null(wb)) wb <- epixlsx_env$report
    if (! is.null(style)) {
    openxlsx::addStyle(wb, onesheet, style, line, col, ...)
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
  
  sheets <- names(report)  
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


# END of SCRIPT  ---------------------------------------------------------- 
