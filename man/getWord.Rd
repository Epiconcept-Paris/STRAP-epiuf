% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{getWord}
\alias{getWord}
\title{Extract a Specific Word from a Text String or List of Text Strings}
\usage{
getWord(tosearch, item = 1, pattern = "\\\\W+")
}
\arguments{
\item{tosearch}{The text string or list of text strings from which to extract the word.}

\item{item}{The position of the word to extract (1-based index). Default is 1.}

\item{pattern}{The pattern used to split the text string into words.
Default is "\\W+" (one or more non-word characters).}
}
\value{
A character string containing the extracted word if \code{tosearch} is a single string,
or a list of extracted words if \code{tosearch} is a list of strings.
}
\description{
This function takes a text string or a list of text strings and extracts a specific word
based on its position, using a given pattern to split the text into words.
}
\examples{
getWord("aaa  bb,cc.",3)
getWord("aaa-bb-cc",2,pattern="-")
}
