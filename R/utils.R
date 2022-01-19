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


#' camel
#' To transform snake_case into CamelCase 
#' 
#' @param x a string or a sring list or a vector of strings 
#'
#' @return a CamelCase value or list 
#' @export  
#'
#' @examples
#' test <- "snake_case"
#' camel(test)
#' 
camel <- function(x){         
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "_"), function(x) paste(capit(x), collapse=""))
}


#' catret
#'    cat ret is a wrapper for cat(...,"newline")
#' @param ... list of values to concatenate for console output
#'
#' @return  nothing
#' @export
#'
#' @examples
#' catret("test")
#' 
catret  <- function(...) {
  cat(...,"\n")
}

# count number of specific char into a text using reg expr
charCount <- function(pattern, stosearch) {
  lengths(regmatches(stosearch, gregexpr(pattern, stosearch)))
  # length(attr(gregexpr(pattern,stosearch)[[1]],
  #            "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}


# right
#
# Extract x rigth characters from a text
#
# @param text Text to extract from
# @param num_char Number of char to extract from rigth
#
# @return  \code{num_char} extracted characters
# @examples
# 
# right("dummy_test",4)
# 
#
right = function (text, num_char) {
  substr(text, nchar(text) - (num_char - 1), nchar(text))
}


left = function (text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

pos <- function(pattern, stosearch) {
  r <- regexpr(pattern, stosearch)
  r <- ifelse(r < 0,0,r)
}

replicate <- function(char, ntime) {
  paste(rep(char, ntime), collapse = "")
}


ask <- function(message,answers) {
  r <- ""
  while(r=="" ){
    n <- readline(message)
    if(!is.na(match(n,as.vector(answers)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    break
  }
}

ok <- function() {
  ask("Do you confirm?", c("Yes", "Y", "y") )
}

bold <- function(...) {
  cat("\033[1m",...,sep="")
}

italic <- function(...) {
  cat("\033[3m",...,sep="")
}

red <- function(...) {
  cat("\033[31m",...,sep="")
}

normal <- function(...) {
  cat("\033[0m",...,sep="")
}



addSep <- function(li,c) {
  sep <- function(x)  paste(x, c)
  li2 <- lapply(li,sep)
  l <- length(li)
  li2[l] <- li[l]
  li2
}



#' @title Remove variables, dataset or functions from memory
#'
#' @description
#' Clear can be used to remove objects from memory (variables, data.frame, functions).
#' Clear is easier than \code{\link{rm}} and is more secure because, by default, it ask for confirmation.
#' Objects to remove can be specified as is or by their name ("character").
#' It's possible to erase all vars, all functions using keywords : "vars" or "functions"
#' "all" keyword will allows total cleaning.
#' @details
#' When keyword or pattern are used and there is more than one object to clear, a confirmation will be issued.
#' Except if noask parameters is set to true
#' If there only one object matching the exactly the \code{what} parameter, this object is removed whithout confirmation
#'
#' @export
#' @importFrom utils ls.str
#' @param what Keyword (vars, functions, all) or pattern
#' @param noask to clear whithout confirmation. Useful when running from a script
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality, available at \url{https://github.com/}.
#' @seealso \code{\link{rm}}
#' @examples
#' tmp <- 5
#' temp <- 5
#' clear(t)
#'
clear <- function(what, noask = FALSE) {
  # arg <- as.list(match.call())
  continue <- TRUE
  if ( missing(what) ) what <- "vars"
  swhat <- as.character(substitute(what))
  if ( length(swhat) > 1 ) {
    swhat <- paste0(swhat[2],swhat[1],swhat[3])
  }
  if ( sum(grep("\\$",swhat) ) > 0 ) {
    cat("To clear a data.frame variable like ")
    italic(swhat)
    normal("  Use dropvar function")
    continue <- FALSE
  }
  # if expr is a variable wich contain char, we can use content of expr ?
  # if (continue & exists(swhat,.GlobalEnv, inherits = FALSE)) {
  #   if (is.character(what) & length(what)==1) {
  #     twhat <- what
  #     swhat <- ifelse(exists(twhat,.GlobalEnv, inherits = FALSE),what,swhat)
  #   }
  # }
  #swhat <- parse(swhat)
  if ( continue ) {
    switch (
      swhat,
      "vars" = { li = setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv, mode = "function")) } ,
      "functions" = { li = ls.str(.GlobalEnv, mode = "function") },
      "all" =  { li = ls((.GlobalEnv)) },
      {
        # there is an objects with that name... we remove it
        if ( exists(swhat) ) {
          li <- c(swhat)
        } else {
          li <- ls(.GlobalEnv, pattern = swhat)
        }
      }
    )
    l <- length(li)
    if (l > 0) {
      lid <- addSep(li,"- ")
      cat(l, " object(s) to remove :")
      italic(as.character(lid))
      normal("\n")
      if ( ( l == 1 & li[1]==swhat ) ||  noask || ok() ) {
        rm(list = li, envir = .GlobalEnv)
      }
    } else {
      cat("No such objets :'")
      italic(swhat)
      normal("'. Use keywords:")
      bold("vars, functions, all")
      normal(" or a pattern (see help)")
    }
    result <- gc()  # garbage collector
  }
}

#' Title
#'    exists look only in GlobalEnv and parent, is.var will search from current and parent until global but not in base
#' @param what An object name to find
#'
#' @return TRUE if found 
#' @export
#' @importFrom utils glob2rx
#'
#' @examples
#' isVar(test)
#' 
isVar <- function(what="") {
  lsfound <- FALSE
  r <- try(mode(what),TRUE)
  if ( ! inherits(r, "try-error")) {
    mwhat <- r
    switch(mwhat ,
           "name" = {
             what <- as.character(substitute(what))
           } ,
           "call" = {
             what <- ""
           } ,
           "function" = {
             what <- ""
           }
    )
    if (length(what) > 1) {
      what <- as.character(substitute(what))
    }
    if ( ! (what == "") ) {
      lsys <- sys.nframe()-1
      what <-glob2rx(what)
      # set_option("last_isvar","")
      for (i in lsys:0)  {
        lc <- ls(sys.frame(i),pattern=what)
        if ( length(lc) > 0 ) {
          r=try(eval(parse(text = lc[1]), sys.frame(i)),TRUE)
          if (! inherits(r, "try-error")) {
            lsfound <- TRUE
            # set_option("last_isvar",r)
          }
        }
      }
    }
  }
  lsfound
}




# END of SCRIPT  --------------------------------------------------------