% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataCheck.R
\name{dataCheck}
\alias{dataCheck}
\title{dataCheck}
\usage{
dataCheck(
  data,
  data_old = NULL,
  cond,
  text = "",
  varname = NULL,
  threshold = NULL
)
}
\arguments{
\item{data}{The dataset}

\item{data_old}{A previously checked dataset}

\item{cond}{The logical condition to test}

\item{text}{The message to prefix the list in case of error}

\item{varname}{The ID varname}

\item{threshold}{The max number of ID displayed}
}
\value{
a vector
}
\description{
dataCheck
}
