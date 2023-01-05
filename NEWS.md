# epiuf (development version)

## Minor improvements

-   Added a `NEWS.md` file to track changes to the package.

## New features

-   In `fillCells()`, added the possibility to add colors.

```{=html}
<!-- -->
```
    # Creating an empty workbook
    wb <- openxlsx::createWorkbook()

    # Creating a new sheet
    openxlsx::addWorksheet(wb, sheetName = "First Sheet")

    # Writing a data frame in the workbook using a style to apply for a set of rows and columns
    alertStyle <- createXlsxStyle(textDecoration = "Bold", fontColour = "red")
    fillCells(wb = wb, 
              onesheet = "First Sheet", 
              line = 7, 
              col = 2, 
              iris, 
              style = alertStyle, 
              styleRowsIndex = which(iris$Sepal.Length<5),
              styleColsIndex = 1:4) 

# epiuf 0.4.0.0 (26-09-2022)

-   Stable Version used on 2022 projects.

# epiuf 0.3.1.0 (08-06-2022)

-   A lot of data dictionary updated.

# epiuf 0.3.0.0 (02-05-2022)

-   Version based on `openxlsx` without Java.
-   Data source and shiny added as a first version.

# epiuf 0.2.0.1 (18-04-2022)

-   Bugs correction after first tests.

# epiuf 0.1.0.0 (08-04-2022)

-   First official release.
