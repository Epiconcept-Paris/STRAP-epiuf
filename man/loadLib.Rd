% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/libutils.R
\name{loadLib}
\alias{loadLib}
\title{loadLib}
\usage{
loadLib(libname)
}
\arguments{
\item{libname}{Name of the package you want to check for install and load}
}
\value{
library name if loaded else false
}
\description{
library loading function which ask for confirmation before installing library
}
\examples{
loadLib("utils")

}
\seealso{
For more details see the help vignette: \cr
\code{vignette("epiuf_package", package = "epiuf")} \cr
\href{"../doc/epiuf_package.html"}{epiuf_package}
}
