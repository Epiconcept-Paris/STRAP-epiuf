% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{push.data}
\alias{push.data}
\title{push.data is used to update globalEnv from function}
\usage{
push.data(name, object)
}
\arguments{
\item{name}{Name of an object to be created or replaced in global env}

\item{object}{any object}
}
\value{
nothing
}
\description{
push.data is used to update globalEnv from function
}
\examples{
push.data("test",6)
rm(test)
}
