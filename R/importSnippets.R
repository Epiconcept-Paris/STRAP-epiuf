#' Project Name : epiuf
#' Script Name  : snippets.R
#' GitHub repo  : STRAP-epiuf
#' Summary      : 
#' Date created : 2023-06-01
#' Author       : Lore Merdrignac
#' Date reviewed:
#' Reviewed by  :

#' Description -------------------------------------------------------------
#' Borrowed and adapted from 
#' https://stackoverflow.com/questions/62220596/can-r-packages-add-code-snippets-to-users-snippet-files

#' Changes Log -------------------------------------------------------------
#' 

# START of SCRIPT  --------------------------------------------------------

#' getOS
#' 
#' Returns the user OS in lower case
#' 
#' @returns character string naming the operator system in use
#' @export
#' @author Lore Merdrignac
#' @seealso [Sys.info()]
#' @examples getOS()
#' 
getOS <- function() {
  info <- Sys.info()
  result <- info[["sysname"]]
  result <- tolower(result)
  return(result)
}


#' Import snippets
#'
#' \code{addPackageSnippets} copies all missing snippet definitions
#'   from 'inst/rstudio/Rsnippets.txt' to the RStudios user snippet location
#'
#' @returns boolean `invisible(FALSE)` if nothing was added, `invisible(TRUE)` if snipped definitions were added
#' 
#' @importFrom rstudioapi versionInfo
#' @importFrom utils tail
#' 
#' @export
#'
#' @examples \dontrun{importSnippets()}
#' 
importSnippets <- function() {
  
  added <- FALSE
  
  # If not on RStudio or RStudioServer exit
  if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    return(NULL)
  }
  
  # Name of files containing snippet code to copy
  pckgSnippetsFiles <- c("Rsnippets.txt")
  
  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  rstudioSnippetsFiles <- c("r.snippets")
  
  # Path to directory for RStudios user files depends on OS
  if (rstudioapi::versionInfo()$version < "1.3") {
    rstudioSnippetsPathBase <- file.path(path.expand('~'),".R", "snippets")
  } else {
    if (.Platform$OS.type == "windows") {
      rstudioSnippetsPathBase <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
    } else {
      rstudioSnippetsPathBase <- file.path(path.expand('~'), ".config/rstudio", "snippets")
    }
  }

  
  # Read each file in pckgSnippetsFiles and add its contents
  for (i in seq_along(pckgSnippetsFiles)) {
    
    # Try to get template, if template is not found skip it
    pckgSnippetsFilesPath <- system.file("snippets", pckgSnippetsFiles[i], package = "epiuf")
    if (pckgSnippetsFilesPath == "") {
      message(paste0("Text file '", pckgSnippetsFiles[i], "' was not found in the folder '\\inst\\snippets' of the epiuf package."))
      next()
    }
    
    # load package snippets definitions
    pckgSnippetsFileContent <- readLines(pckgSnippetsFilesPath, warn = FALSE)
    
    # Extract names of package snippets
    pckgSnippetsFileDefinitions <- pckgSnippetsFileContent[grepl("^snippet (.*)", pckgSnippetsFileContent)]
    
    
    # Construct path for destination file
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, rstudioSnippetsFiles[i])
    
    # If targeted RStudios user file does not exist, raise error (otherwise we would 'remove')
    # the default snippets from the 'user file'
    if (!file.exists(rstudioSnippetsFilePath)) {
      stop(paste0( "'", rstudioSnippetsFilePath, "' does not exist yet.\n",
                   "Use 'RStudio -> Tools -> Global Options -> Code -> Edit Snippets' \n",
                   "to initalize user defined snippets file by adding dummy snippet.\n"))
    }
    
    # Extract 'names' of already existing snippets
    rstudioSnippetsFileContent <- readLines(rstudioSnippetsFilePath, warn = FALSE)
    rstudioSnippetDefinitions <- rstudioSnippetsFileContent[grepl("^snippet (.*)", rstudioSnippetsFileContent)]
    
    # replace two spaces with tab, ONLY at beginning of string
    pckgSnippetsFileContentSanitized <- gsub("(?:^ {2})|\\G {2}|\\G\t", "\t", pckgSnippetsFileContent, perl = TRUE)
    
    # find definitions appearing in packageSnippets but not in rstudioSnippets
    # if no snippets are missing go to next file
    snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    if (length(snippetsToCopy) == 0) {
      cat(paste0("Following snippets will NOT be added because there is already an existing snippet with that name:\n",
                 paste0(snippetsNotToCopy, collapse=", ") ,"."))
      next()
    }
    
    # Inform user about changes, ask to confirm action
    if (interactive()) {
      cat(paste0("You are about to add the following ", length(snippetsToCopy),
                 " snippet(s) to '", rstudioSnippetsFilePath, "':\n",
                 paste0(paste0("-", snippetsToCopy), collapse="\n")))
      if (length(snippetsNotToCopy) > 0) {
        cat(paste0("\n(The following snippets will NOT be added because there is already an existing snippet with that name:\n",
                   paste0(snippetsNotToCopy, collapse=", ") ,")"))
      }
      answer <- readline(prompt="Do you want to proceed (y/n): ")
      if (tolower(substr(answer, 1, 1)) != "y") {
        message("None of the snippet(s) '", paste0(snippetsToCopy, collapse = ", "), "' were added.")
        next()
      }
    }
    
    # Create list of line numbers where snippet definitons start
    # This list is used to determine the end of each definition block
    allPckgSnippetDefinitonStarts <- grep("^snippet .*", pckgSnippetsFileContentSanitized)
    
    for (s in snippetsToCopy) {
      startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)
      
      # Find last line of snippet definition:
      # First find start of next definition and return
      # previous line number or lastline if already in last definition
      endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] - 1
      if (is.na(endLine)) {
        endLine <- length(pckgSnippetsFileContentSanitized)
      }
      
      snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")
      
      # Make sure there is at least one empty line between entries
      # if (!(utils::tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n = 1) %in% c("", "\n", "\t"))) {
      #   snippetText <- paste0("\n", snippetText)
      # }
      if (utils::tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n = 1) != "") {
        snippetText <- paste0("\n", snippetText)
      }
      
      # Append snippet block, print message
      cat(paste0(snippetText), file = rstudioSnippetsFilePath, append = TRUE)
      cat(paste0("* Added '", s, "' to '", rstudioSnippetsFilePath, "'\n"))
      added <- TRUE
    }
  }
  
  if (added) {
    message("Restart RStudio to use new added snippet(s)")
  }
  
  return(invisible(added))
  
}


# END of SCRIPT  ----------------------------------------------------------
