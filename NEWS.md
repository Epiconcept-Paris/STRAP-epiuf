# epiuf 0.5.1.1 (2024-02-26)

## New features

-   Set of functions to automate the process of data extraction from REDCap, using REDCap API and 'keyring' R package to securely handle secrets (issue #65)
    -   accessREDCap
    -   downloadREDCap
    -   createKeyring / modifyKeyring / deleteKeyring / deleteKeyringAll
    -   existsKeyring
    -   grabKeyring
    -   listKeyring
    -   setOrGetFilePath

# epiuf 0.5.1.0 (2023-12-19)

## New features

-   epiuf::RefSite data frame: correspondence table with country/site labels and code (e.g., Serbia \>\> RS)
-   epiuf::DummyData data frame: dummy dataset with random case based data (VE TND study design style) useful for testing
-   Set of new functions 'FindAndReplace' to search for a specific pattern in files and replace it if necessary

## Improvements

-   Optimisation of applyNAAll function (issue #101)

## Minor improvements and bug fixes

-   Bug fixed in opendictionary (issue #103)
-   Improvement getDelayIf (issue #102)
-   Improvements in dictionary functions (issue #96)

# epiuf 0.5.0.1 (2023-09-06)

## Minor improvements

-   epitable:
    -   All combinations of TRUE and FALSE inputs for arguments 'col', 'row', 'total' and 'missing' now work
    -   Option 'perc' added that enables to output percentages from the whole data

# epiuf 0.5.0.0 (02-08-2023)

## New features

-   utilsMaths script including new functions: getMax, getMin, getMean, getMedian
-   importSnippets: function to import EpiConcept recommended snippets
-   validDate : function to guess string date format from a dataset and return the corresponding date format
-   utilsDate script including new function: lastDateMonth
-   test unit added for function validDate and functions from utilsMath and utilsDate

## Minor improvements

-   Help improved for utility functions (countIf, string, path, etc.)
-   Epitable return a basic datatable for custom display use (similar to crosstab)

## Minor bug fixes

-   Minor bug fix in countIf (duplicate), listIf, listVar and printVar

## Obsolete functions

-   comp and moyenne

# epiuf 0.4.1.0 (13-01-2023)

## Minor improvements

-   Added a `NEWS.md` file to track changes to the package.

## New features

-   In `fillCells()`, added the possibility to add colors with three new arguments: `style`, `styleRowsIndex` and `styleColsIndex`.
-   In `setPath()`, added the possibility to create sub-folders.

## Minor bug fixes

-   `setPath()`

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
