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

#' cleanNames
#'
#' @param name A string to clean by removing non ascii char and accents
#' @param chr A chr used to replace non ascii character  
#'
#' @return the cleaned string
#' @export
#'
#' @examples
#' x <- "ÁbcdêãçoàúüEssai/=+$67"
#' cleanNames(x)  # no libraries needed

cleanNames <-  function(name,chr="") {
  # will remove all punctuation defined as  "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
  # if you want to keep only non accent alpha numeric use : [^a-zA-Z0-9] or [^[:alnum:]]
  # here we use iconv to transform accent char to simple ascii 
  
  name <- iconv(name, from="", to="ASCII//TRANSLIT") # will replace  accentued with ascii 
  gsub("[[:punct:]]", chr, name)  # no libraries needed
  
}

# x <- "ÁbcdêãçoàúüEssai/=+$67"
# x <-  iconv( x , from="", to="ASCII//TRANSLIT")
# gsub("[^a-zA-Z0-9]", "",x )  # no libraries needed


#' Change the name of a data.frame column
#'
#' @param data Name of the column/variable to rename
#' @param oldname Name of the column/variable to rename
#' @param newname New name to apply
#'
#' @return Message to confirm the change
#' @export
#'
#' @examples
#' df <- as.data.frame( c(One=1,Two=2) )
#' rename(df,Two,Last)
rename <- function(data, oldname, newname) {
  # r <- as.list(match.call())
  # old <- getvar(r$oldname)
  # if (! is.null(old) ) {
  #   old.fname <- getvar()
  #   old.name <- getvarname()
  #   dfname <- get_lastdfname()
  #   df <- getlastdf()
    dfname <- as.character(substitute(data))
    oldname <- as.character(substitute(oldname))
    newname <- as.character(substitute(newname))
    lname <- names(data)
    lname[lname==oldname] <-  newname
    names(data)<-lname
    push.data(dfname,data)
    
    bold(oldname)
    cat(" renamed as ")
    bold(newname)
    catret("")
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


#' lpad
#'      Used to display value with a fixed width format
#'      format value according to width and digit is value is a number 
#'
#' @param value A value to format
#' @param width The expected width
#' @param digit The number of digit
#'
#' @return The formated value
#' @export
#'
#' @examples
#' lpad("test",10,0)
#' lpad(2,6,2)
lpad <- function(value,
                 width = 11,
                 digit = 0) {
  if (is.numeric(value) ) {
    r <-
      format(round(value, digits = digit),
             width = width ,nsmall = digit ,
             justify = "right")
  } else {
    r <-
      format(value, width = width , justify = "right")
  }
  if (is.character(value) & max(nchar(r)) > width) {
    for ( i in 1:length(r) ) {
      if (nchar(r[i])>width) {
        r[i] <- paste0(substr(r[i], 1, width - 2), "..")
      }
    }
  }
return(r)
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
#' @param what an object name, a Keyword (vars, functions, all) or a pattern
#'             Object name can be passed for evaluation, pattern has to be quotted 
#'              
#' @param noask to clear whithout confirmation. Useful when running from a script
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality, available at \url{https://github.com/}.
#' @seealso \code{\link{rm}}
#' @examples
#' tmp <- 5
#' temp <- 5
#' temp2 <- 6
#' clear(tmp)
#' clear("t*")
clear <- function(what="all", noask = FALSE) {
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
      { # no keyword then we look for specific objects
        # there is an objects with that name... we remove it
        if ( exists(swhat) ) {
          li <- c(swhat)
        } else {
          spattern <- glob2rx(swhat)
          li <- ls(.GlobalEnv, pattern = spattern)
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
#' @return list of the variables matching the pattern   
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
#  utils::head(dataset[,grepl(pattern,names(dataset))])
  lvar <- grepl(pattern,names(dataset))
  names(dataset)[lvar==TRUE]
}

#' printVar
#'
#' @param dataset A data.frame 
#' @param pattern A pattern for varname in the dataframe see \code{regex}  
#' @param regex Should the pattern be used as regex expression or use classical "joker" ? and * 
#'
#' @return  A data frame of 10 rows with selected columns 
#' @export
#'
#' @examples
#'  data <- data.frame(Id = 1:4 ,  
#'                    vaccage = c(34,45, 50,22 ),
#'                    symp = c("Y","Y","N","N"),
#'                    vaccboost=c("N","Y","N","Y"))
#'printVar(data,"symp")
#'printVar(data,"vac*")                    
printVar <- function(dataset,pattern,regex=FALSE) {
  lvar <- listVar(dataset,pattern,regex)
  ldata <- as.data.frame(dataset[,lvar])
  colnames(ldata) <- lvar
  utils::head(ldata)
}


#' dropVar
#'
#' @param data A dataset
#' @param varname A column to remove 
#'
#' @return The dataset 
#' @export
#'
#' 
dropVar <- function(data , varname) {
  dfname <- as.character(substitute(data))
  r <- as.list(match.call())
  vartodrop <- as.character(r$varname)
  if (vartodrop %in% names(data)) {
    # we drop from data copy
    data[,vartodrop] <- NULL
    # feedback for user
    cat("Column ")
    bold(vartodrop)
    cat(" dropped from ")
    bold(dfname)
    catret("")
    # update original data.frame
    push.data(dfname,data)
  } else red(vartodrop, "not found in",dfname)
}

#' push.data is used to update globalEnv from function
#'
#' @param name Name of an object to be created or replaced in global env
#' @param object any object 
#'
#' @return nothing
#' @export
#'
#' @examples
#' push.data("test",6)
#' rm(test)
push.data <- function(name,object) {
  exp = call("<-",name,object)
  eval(exp,envir=.GlobalEnv)
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


# given a column name, finddf retrieve all df containing that column
# mainly used by getvar in short syntax
finddf <- function(varname) {
  .df <-
    names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
  ndf <- length(.df)
  j <- 1
  nfound <- 0
  dffound <- ""
  dflist <- list()
  while (j <= ndf) {
    pat <- paste0("^",varname,"$")
    ifound <- grep(pat, names(get(.df[j])))
    if (length(ifound) > 0) {
      nfound <- nfound + 1
      dflist[nfound] <- .df[j]
      # list of dataset containing varname
      dffound <-
        paste0(dffound, ifelse(dffound == "", "", ", "), .df[j])
    }
    j <- j + 1
  }
  r <- list()
  r$count <- nfound
  r$namelist <- dflist
  r$namestring <- dffound
  return(r)
}

# retrieve the default data.frame defined by setdata
# getdata return the df if there is only one in memory
getdata <- function() {
  df <- getEpiOption("dataset")  # epif_env$dataset
  if ( is.character(df) ) {
    if (! df == "") {
      # dataset contain name ... then get the data.frame
      df <- get(df)
      # df <- eval(parse(text = df))
    }
  }
  # we verify that we finally have a dataframe
  if ( ! is.data.frame(df)) {
    df <- NULL
  }
  # if no dataframe set by default and one is available in global env, then we use it
  if (is.null(df)) {
    list_df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
    ndf <- length(list_df)
    if (ndf == 1) {
      df <- get(list_df[1])
    }
  }
  df
}



# internal function to retrieve dataset variables

#' @title retrieve a data.frame column
#'
#' @param what Name of the column
#'
#' @return The column
#' @export
#' @importFrom utils glob2rx
#'
#' @examples
#' getvar()
getvar <- function(what = NULL) {
  
  # first, if what is missing we return previous one
  if (missing(what)) {
    return(getEpiOption("last_var"))
  } else {
    argpassed <- substitute(what)
    # should we look at var content ??
    # subst <- FALSE
    # if var is char content is used
    # if (exists(var)) {
    #   if (is.character(varname) & length(varname)== 1 ) {
    #      var<-eval(varname)
    #      subst<-TRUE
    #   }
    # }
    # reset of global vars
    # resetvar()
    iscol <- FALSE
    dfname <- ""
    # Look at type of argument and get a working version of it
    r <- try(mwhat <- mode(what),TRUE)
    if (inherits(r, "try-error")) {
      varname <- deparse(substitute(what))
    } else {
      switch(mwhat ,
             "character" = {
               varname <- what
             } ,
             "call" =  {
               varname <- deparse(what)
             } ,
             "name" = {
               varname <- as.character(what)
             } ,
             { # else
               varname <- deparse(argpassed)
             }
      )
      iscol <-  TRUE
    }  
    # got it, we save the name
    
    # epif_env$last_var <- varname
    # epif_env$last_varname <- varname
    # if ( (l <-pos("\\$",varname)) > 0) {
    #   epif_env$last_varname <- substring(varname,l+1)
    #   epif_env$last_df <- substr(varname,1,l-1)
  }
  
  # just create an expression with content
  ex <- parse(text=varname)
  # we can test isVar
  # var doesn't exist.. may be it's a formula ? We try to eval but we catch error
  continue <- FALSE
  r <- try(eval(ex), TRUE)
  if (!inherits(r, "try-error")) {
    # it's a formula ... it's evaluation is returned if not a function
    if ( ! mode(r) == "function" ) {
      return(r)
    } else {
      #  in that situation we can look for column name... to be modified
      warning(
        paste(
          varname ,
          "is probably not a variable but a function"),
        call. = FALSE
      )
    }
  } else continue <- TRUE
  if (continue) {
    # may be varname is part of a dataset ?
    dffound <- finddf(varname)
    # only one ? great
    if (dffound$count > 1) {
      dfset <- setdata()
      if (!dfset=="") {
        lset <- dfset %in% dffound$namelist
        if (lset) {
          dfname  <- dfset
        }
      }
    }
    if (dffound$count == 1) {
      dfname <- dffound$namelist[[1]]
    }
    if (!dfname=="") {
      varfullname <- paste(dfname, "$", varname , sep = "")
      # we update varname with data.frame value
      # epif_env$last_var <- varfullname
      # epif_env$last_varname <- varname
      # epif_env$last_df <- dfname
      r <- try(eval(parse(text =varfullname)),TRUE)
      return(r)
    } else if (dffound$count > 1){
      warning(
        paste0(
          varname ,
          " is an ambiguous name and exists in following datasets: ",
          dffound$namestring,"\n","You could try ",dffound$namelist[[1]],"$",varname,
          "\n or try to use setdata(",dffound$namelist[[1]],")"
        ),
        call. = FALSE
      )
      # resetvar()
      return(NULL)
    } else {
      warning(paste(varname , "is not defined as variable or data.frame column"), call. = FALSE)
      return(NULL)
    }
  } # var not exists
} # not missing


#' @title set or retrieve the default data.frame
#'
#' Set a data.frame as default data.frame for epifield function. This avoid typing
#' and simplify syntax for R newcomers. setdata is mandatory for some functions :
#' generate, countif
#' If missing df name, then setdata() return the current data.frame name
#'
#' @param df Name of the data.frame to set as default
#' @export
#' @return  The current data.frame name
#' @examples
#' df <-as.data.frame(c(1,2))
#' setdata(df)
#' rm(df)
#'

setdata <- function(df = NULL) {
  # if argument is NULL setdata return the current default data frame
  if (missing(df)) {
    return(getEpiOption("dataset"))
  } else {
    # argument is a dataframe ?
    m_df <- try(is.data.frame(df),TRUE)
    if ( ! inherits(m_df, "try-error")) {
      # df exists as an object
      # if TRUE then it is a data frame
      if (m_df) {
        # setdata as a meaning only if the passed dataframe exist in environment
        c_df <- as.character(substitute(df))
        # the name is searched in global env
        if (sum(match(ls.str(.GlobalEnv, mode = "list"), c_df), na.rm = TRUE) > 0) {
          cat("Default data frame is now set to",c_df)
          setEpiOption("dataset", c_df )
        } else {
          stop("Data frame should exist in global environnment")
        }
        # df is not a data frame, if arg is character, we search for a dataset named df
      } else if (is.character(df)) {
        # if df is empty then we cancel the default dataframe
        if (df=="") {
          setEpiOption("dataset", df)
          cat("setdata cleared")
        } else if (exists(df)) {
          # an object named df exist, is it a data frame ?
          if (is.data.frame(get(df))) {
            setEpiOption("dataset", df)
            cat("Default data frame is now set to",df)
          } else stop(df , " is not a data.frame")
        } else stop(df , " doesn't exist in environment")  # no object with that name
      }
    } else {
      # a data frame was passed directly as argument
      stop("Data frame should exist in global environnment")
    }
  }
}




# END of SCRIPT  --------------------------------------------------------