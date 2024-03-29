% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validDate.R
\name{validDate}
\alias{validDate}
\title{validDate}
\usage{
validDate(datevar, format = NULL, dropFuture = TRUE)
}
\arguments{
\item{datevar}{A character vector to be converted into a Date vector.}

\item{format}{An optional format string to use instead of the automatic system. Useful for complex formats.}

\item{dropFuture}{Logical value. If TRUE and a 2 digit year in the futur (greater than system date),
it will be placed in the past by substrating one century . Default is TRUE.}
}
\value{
A Date object or a vector of Date objects in standard R date format.
}
\description{
Description: Validates and converts date formats to standard R date format
This function processes a character or a vector of characters to verify the date format
and transform it into a standard R date format.
If the format cannot be automatically determined, you can provide a format string as a parameter.
}
\examples{
validDate("2023-05-25")
validDate(c("20221120","20210615","20210303","",NA))
validDate(c("05-25-2023", "26/05/2023"), format = "\%m-\%d-\%Y")
validDate(datevar = "15/01/60",dropFuture = FALSE)
}
\seealso{
For more details see the link below to access the vignette:
\href{../doc/epiuf_package.html}{\code{vignette("epiuf_package")}}
}
\author{
STRAP team \email{strap@epiconcept.fr}
}
