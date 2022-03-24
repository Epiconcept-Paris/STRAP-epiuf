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

#' charCount
#' count number of specific char into a text using reg expr
#' 
#' @param pattern "The character or pattern to searh
#' @param stosearch The string to search in 
#'
#' @return number of match
#' @export
#'
#' @examples
#' nb <- charCount("/", "test/essai/try")
#' 
charCount <- function(pattern, stosearch) {
  # pattern <- glob2rx(pattern)
  lengths(regmatches(stosearch, gregexpr(pattern, stosearch)))
  # length(attr(gregexpr(pattern,stosearch)[[1]],
  #            "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}


#' right
#'
#' Extract x rigth characters from a text
#'
#' @param text Text to extract from
#' @param num_char Number of char to extract from rigth
#'
#' @return  \code{num_char} extracted characters from right side
#' @export
#' @examples
#' 
#' right("dummy_test",4)
#' 
#'
right = function (text, num_char) {
  substr(text, nchar(text) - (num_char - 1), nchar(text))
}


#' Left
#'
#' @param text Text to extract from 
#' @param num_char number of char to extract
#'
#' @return  \code{num_char} extracted characters from left side
#' @export
#'
#' @examples
#' left("dummy_test",4)
#' 
left = function (text, num_char) {
  substr(text, 1, num_char)
}

#' mid
#'
#' @param text Text to extract from 
#' @param start_num start of extraction
#' @param num_char number of char to extract
#'
#' @return \code{num_char} extracted characters starting \code{start_num}
#' @export
#'
#' @examples
#' mid("dummy_test",7,4)
#' 
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

#' pos
#'
#' @param pattern A pattern to search in stosearch
#' @param stosearch A character string 
#'
#' @return Position of pattern in stosearch
#' @export
#'
#' @examples
#' pos("/","test/string")
#' 
pos <- function(pattern, stosearch) {
  r <- regexpr(pattern, stosearch)
  r <- ifelse(r < 0,0,r)
}

#' replichar
#'
#' @param char Character to replicate
#' @param ntime Number of replication
#'
#' @return \code{char} replicated \code{ntime} 
#' @export
#'
#' @examples
#' replichar("-",60)
#' 
replichar <- function(char, ntime) {
  paste(rep(char, ntime), collapse = "")
}


askinput <- function(message,answers) {
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

#' yesno
#' 
#' Dispkay a prompt and wait for a Yes/No answer
#'
#' @param message The prompt to display before the Yes/No choice
#'
#' @return  True if yes, FALSE if no and NA otherwise
#' @importFrom utils askYesNo
#' @export
#'
#' 
yesno <- function(message) {
  askYesNo(message,NA,"Yes/No/Cancel")
}

#' confirm 
#' Confirmation dialog box
#'
#' @param message The prompt to display before confirmation
#' 
#' @return logical True if answer is "Yes"
#' @export
#'
#' 
confirm <- function(message="") {
  askinput(paste(message," (Press Y and [enter] to confirm) : "), c("Y", "y") )
}


#' bold
#'
#' @param ... values to be outputted in bold
#'
#' @return nothing
#' @export
#'
#' @examples
#' bold("text in bold")
bold <- function(...) {
  if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    cat("\033[1m",...,"\033[0m",sep="")
  } else if (knitr::is_html_output()) {
    r <-  paste0(...)
    sprintf('<span style="font-weight:bold;">%s</span>', r)
  }      
}

#' italic  
#'
#' @param ... values to be outputted in italic
#'
#' @return nothing
#' @export
#'
#' @examples
#' italic("text in italic")
italic <- function(...) {
  if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    cat("\033[3m",...,"\033[0m",sep="")
  } else if (knitr::is_html_output()) {
    r <-  paste0(...)
    sprintf('<span style="font-style:italic;">%s</span>', r)
  }      
  
}

#' red
#'
#' @param ... values to be outputted in red
#'
#' @return nothing
#' @export
#'
#' @examples
#' red("text in red")
red <- function(...) {
  if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    cat("\033[31m",...,"\033[0m",sep="")
  } else if (knitr::is_html_output()) {
    r <-  paste0(...)
    sprintf('<span style="color:red;">%s</span>', r)
  }  
}


#' blue
#'
#' @param ... values to be outputted in blue
#'
#' @return nothing
#' @export
#'
#' @examples
#' blue("text in blue")
blue <- function(...) {
  if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    cat("\033[34m",...,"\033[0m",sep="")
  } else if (knitr::is_html_output()) {
    r <-  paste0(...)
    sprintf('<span style="color:blue;">%s</span>', r)
  }      
  
}
                                                                    

#' addSep
#' transform a list into a string of separated values
#'
#'
#' @param li A list of values
#' @param c A separator
#'
#' @return A string representing the list as separated values
#' @export
#'
#' @examples
#' li <- list("one","two","three")
#' addSep(li,",")
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
    cat("  Use dropvar function")
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
      catret("")
      if ( ( l == 1 & li[1]==swhat ) ||  noask || confirm() ) {
        rm(list = li, envir = .GlobalEnv)
      }
    } else {
      cat("No such objets :'")
      italic(swhat)
      cat("'. Use keywords: ")
      bold("vars, functions, all")
      cat(" or a pattern (see help)")
    }
    result <- gc()  # garbage collector
  }
}


#' listVar
#'
#' @param dataset A dataset to explore
#' @param pattern Pattern representing varname
#' @param regex  Should the pattern be used as regex expression or use classical "joker" ? and * 
#'
#' @return head of the dataset for the selected variables  
#' @export
#'
#' @examples
#' data <- data.frame(Id = 1:4 ,  
#'                    vaccage = c(34,45, 50,22 ),
#'                    symp = c("Y","Y","N","N"),
#'                    vaccboost=c("N","Y","N","Y"))
#'listVar(data,"symp")
#'listVar(data,"vac*")                    
#'                    
#' 
listVar <- function(dataset,pattern,regex=FALSE) {
  if (!regex){pattern <- glob2rx(pattern)}
  utils::head(dataset[,grepl(pattern,names(dataset))])  
}




#' isVar fonction WIP do not use
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
           },
           what <- as.character(substitute(what))
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