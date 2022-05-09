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
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------
# creating a global from the package doesn't work like this, the code should be in a function 
# which is call or in zzz.R (onload, onattach) 
# global <<- list()
# global$epiuf <- TRUE



# epifield envirronement used to manage epifield options
epiutils_env <- new.env(parent = emptyenv())

# options for epifield
# default option controling output of R Code when usefull
epiutils_env$show_Rcode <- FALSE

epiutils_env$stat_digits <- 2

# global to retrieve current and last selection in short syntax system
# The current selection applied to the current dataframe
epiutils_env$select <- ""

#' getEpiOption
#'
#' retrieve an epiutils package option or parameter
#'
#' @param EpiOption name of the option to retrieve
#' @export
#' @return  option value
#' @examples
#' getEpiOption("optionname")
#'
#'
getEpiOption <- function(EpiOption) {
  
  s_opt <- substitute(EpiOption)
  s_opt <- as.character(s_opt)
  
  if (exists(s_opt)) {
    if (is.character(EpiOption)) {
      s_opt <- EpiOption
    }
  }
  else {
    if (is.character(EpiOption)) {
      s_opt <- EpiOption
    }
  }
  if (match(s_opt, ls(envir = epiutils_env), nomatch = 0)) {
    eval(parse(text = paste0("epiutils_env$", s_opt)))
  } else {
    # warning("Option unavailable")
    r <- NULL
  }
}


#' listEpiOption
#'
#' @param pattern A pattern for options to be listed
#'
#' @return List of options defined with setEpiOption
#' @export
#'
#' @examples
#' 
#'  listEpiOption()
#'

listEpiOption <- function(pattern="*") {
  pattern <- glob2rx(pattern)
  OptionList <- ls(envir = epiutils_env,all.names=TRUE,pattern=pattern)
  sapply(OptionList, function(x) getEpiOption(x))
}


#' setEpiOption
#'
#' assign a package option
#'
#' @param EpiOption name of the option to assign as character
#' @param value The value to be assigned to option
#' @export
#' @return  The previous option value before new assignment
#' @examples
#' setEpiOption("option",1)
#'
#'

setEpiOption <- function(EpiOption, value) {
  # we get op as symbol
  s_op <- deparse(substitute(EpiOption))
  # if op is a variable wich contain char, we use content of op
  ok <- FALSE
  tryCatch(
    if (is.character(EpiOption)) {
      s_op <- EpiOption
      ok <- TRUE
    }
    , error = function(c) { }
  )
  
  old <- NA
  
  eval(parse(text = paste0("old <- epiutils_env$", s_op)))
  eval(parse(text = paste0("epiutils_env$", s_op , "<- value")))
  
  invisible(old)
}

# internal used to reset the short syntax system
resetvar <- function() {
  setEpiOption("last_var","")          # last argument object name (in long syntax)
  setEpiOption("last_isvar","")        # last argument is a column name ?
  setEpiOption("last_varname","")      # if yes, varname is the column name
  setEpiOption("last_df", "")           # if yes, df is the last dataframe
  setEpiOption("last_error", NA)
  setEpiOption("dataset","")
}

resetvar()


# END of SCRIPT  --------------------------------------------------------