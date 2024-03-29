% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilsDate.R
\name{lastDateMonth}
\alias{lastDateMonth}
\title{lastDateMonth}
\usage{
lastDateMonth(month, lc_time = "C")
}
\arguments{
\item{month}{character string, abbreviated month+year (eg. dec2022)}

\item{lc_time}{character string, input of the \code{Sys.setlocale("LC_TIME", lc_time)},
required if the system local language for the time is not the same as the \code{month} argument
(e.g., if Sys.getlocale("LC_TIME") is "French_France.utf8", but month = "dec2022").
Default value being "C" for English format.}
}
\value{
The last date in that month in date format
}
\description{
Takes monthly dates in character string such as "dec2022" and returns the date of
the last day of the month in date format
}
\examples{
lastDateMonth("dec2022")

}
\author{
Jenny Howard \email{j.howard@epiconcept.fr}
}
