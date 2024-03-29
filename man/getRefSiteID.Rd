% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RefSite.R
\name{getRefSiteID}
\alias{getRefSiteID}
\title{Get the ID code corresponding to the site Label or LocationName}
\usage{
getRefSiteID(Label, LocationName)
}
\arguments{
\item{Label}{Character string to search in the \code{"Label"} column from \code{epiuf::RefSite} reference table}

\item{LocationName}{Character string to search in the \code{"LocationName"} column from \code{epiuf::RefSite} reference table}
}
\value{
Character string or a data frame if multiple answers
}
\description{
Get the ID code corresponding to the country/study site label from \code{epiuf::RefSite} reference table
}
\examples{
\dontrun{
getRefSiteID()
}
getRefSiteID(Label = "Austria")
getRefSiteID(LocationName = "Bosnia and Herzegovina")
getRefSiteID(Label = "The Czech Republic", LocationName = "Czechia")
\dontrun{
getRefSiteID(Label = "Czechia")
}

}
\seealso{
\itemize{
\item \code{\link{getRefSiteLabel}}
\item \code{\link{getRefSiteLocName}}
}
For more details see the corresponding vignette: \code{vignette("RefSite")}
}
\author{
Lore Merdrignac \email{l.merdrignac@epiconcept.fr}
}
