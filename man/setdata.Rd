% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{setdata}
\alias{setdata}
\title{setdata}
\usage{
setdata(df = NULL)
}
\arguments{
\item{df}{Name of the data.frame to set as default}
}
\value{
The current data.frame name
}
\description{
set or retrieve the default data.frame

Set a data.frame as default data.frame for epifield function. This avoid typing
and simplify syntax for R newcomers. setdata is mandatory for some functions :
generate, countif
If missing df name, then setdata() return the current data.frame name
}
\examples{
df <-as.data.frame(c(1,2))
setdata(df)
rm(df)

}
