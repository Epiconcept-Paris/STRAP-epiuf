% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{addSep}
\alias{addSep}
\title{Add a separator between elements of a character list}
\usage{
addSep(charlist, sep = ", ")
}
\arguments{
\item{charlist}{Character list to which the separator should be added.}

\item{sep}{The separator string to be added between elements. Default is ", ".}
}
\value{
A single character string where elements from \code{charlist} are separated by \code{sep}.
}
\description{
This function takes a character list and adds a separator string between each element.
The last element is not followed by a separator.
}
\examples{
li <- list("one","two","three")
addSep(li,"-")
addSep(li)
}
