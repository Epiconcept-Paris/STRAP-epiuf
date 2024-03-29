% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilsFiles.R
\name{fileName}
\alias{fileName}
\title{Extract File Base Name Without Extension From File Path or URL}
\usage{
fileName(text)
}
\arguments{
\item{text}{Character. The full path or URL of the file.}
}
\value{
Character. The base name of the file without the extension.
}
\description{
This function takes a file path or URL and returns the base name of the file without its extension.
Unlike \code{basename} from base R, which returns the file name with the extension, this function
directly provides just the base name, making it easier to manipulate or compare file names.
}
\examples{
fileName("path/to/file.txt")  # Returns "file" instead of "file.txt"
fileName("https://example.com/file.csv")  # Returns "file" instead of "file.csv"

}
