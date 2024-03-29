% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasourcestools.R
\name{updateSourceData}
\alias{updateSourceData}
\title{updateSourceData}
\usage{
updateSourceData(
  sourcefilename,
  status = c("NEW", "UPDATED", "IMPORTED", "CHECKED", "VALIDATED", "POOLED"),
  sourcetype = NULL,
  date = Sys.time(),
  nbrecords = NULL
)
}
\arguments{
\item{sourcefilename}{Name of the source file to update with new informations}

\item{status}{: IMPORTED/CHECKED/VALIDATED/POOLED}

\item{sourcetype}{optional source type}

\item{date}{Date to be used as timestamp}

\item{nbrecords}{Optional number of records to add to the file information}
}
\value{
nothing
}
\description{
updateSourceData
}
\examples{
\dontrun{ 
# update status when file is imported 
updateSourceData(filename,status="IMPORTED") 
}
}
