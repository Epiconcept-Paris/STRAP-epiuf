% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilsFiles.R
\name{replaceStr}
\alias{replaceStr}
\title{Replace Substring in a Given String}
\usage{
replaceStr(pattern, replacement, intext, word = FALSE, ignore.case = TRUE)
}
\arguments{
\item{pattern}{The substring that you want to replace.}

\item{replacement}{The new substring that will replace \code{pattern}.}

\item{intext}{The input text where you want to replace the substring.}

\item{word}{Logical, if TRUE only whole words will be replaced.}

\item{ignore.case}{Logical, if TRUE the function ignores case.}
}
\value{
A string with the replaced substrings.
}
\description{
This function replaces occurrences of a given substring (\code{pattern}) in a text (\code{intext}) with a new substring (\code{replacement}).
You can also specify if you want to replace only whole words and whether to ignore case.
}
\examples{
replaceStr( "world", "everyone","Hello world")
replaceStr("WORLD", "everyone","Hello world",  ignore.case = TRUE)
replaceStr( "world", "everyone","Hello world", word = TRUE)
}
