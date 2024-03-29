% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq.R
\name{freq}
\alias{freq}
\title{Frequency distribution.}
\usage{
freq(x, y = NULL, missing = FALSE, quietly = FALSE)
}
\arguments{
\item{x}{As numbers, factors or text.}

\item{y}{As numbers, factors or text.}

\item{missing}{If false then missing values are not included in the table
A summary output of number of missing values is added at the end}

\item{quietly}{No output, only return value}
}
\value{
An array containing  values of \code{...}   \code{}
}
\description{
\code{freq} Display a frequency distribution.
#'
}
\examples{
freq(c(3,1,2,2,5))


}
\references{
Based on: \emph{Epi6} and \emph{Stata} functionnality,
available at \url{https://github.com/}.
}
\seealso{
\code{\link{table}} for 2by2 tables
}
\author{
Gilles Desve
}
