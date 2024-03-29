% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{charCount}
\alias{charCount}
\title{Count the Occurrences of a Pattern in Text}
\usage{
charCount(pattern, stosearch, ignore.case = FALSE)
}
\arguments{
\item{pattern}{Character. The pattern to search for within the text string(s).}

\item{stosearch}{Character. The text string(s) in which to search for the pattern.}

\item{ignore.case}{Logical. If TRUE, the search is case-insensitive. Default is FALSE.}
}
\value{
Integer. The total count of occurrences of the pattern in the text string(s).
}
\description{
Counts the number of times a specified pattern appears in a given text string or vector of strings.
If the pattern is a single character or a sequence of characters, the function returns
the number of occurrences of the pattern in "stosearch".
}
\details{
When the pattern is a regular expression, the function returns the number of times the expression
matches in "stosearch".
For specific regex patterns like ".", you should escape it: e.g., charCount("\\.txt", c("test.txt", "sample.txt")).
}
\examples{
charCount("a", "banana")  # Returns 3 (three 'a's in "banana")
charCount("a", c("banana", "apple"))  # Returns 4 
charCount("\\\\d", "13ab2c")  # Returns 3 (three digits in "13ab2c")
charCount("\\\\.txt", c("test.txt", "sample.txt"))

}
\seealso{
\code{\link{nchar}}
}
