% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readData.R
\name{readData}
\alias{readData}
\title{Title}
\usage{
readData(
  filename = "",
  factorise = FALSE,
  lowercase = FALSE,
  label = NULL,
  sheet = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{filename}{Name of the file to read/import}

\item{factorise}{Should character value be factorised, default = FALSE}

\item{lowercase}{Should variable names appear in lower case or as is , default to lowercase}

\item{label}{Not used currently}

\item{sheet}{Name of the excel sheet to be read}

\item{verbose}{Print or not a message when data imported}

\item{...}{Other parameters passed to the base function}
}
\value{
The dataset read from file
}
\description{
Title
}
\examples{
readData("flucases.csv")


}
