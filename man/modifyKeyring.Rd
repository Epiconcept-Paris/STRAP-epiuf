% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Keyring.r
\name{modifyKeyring}
\alias{modifyKeyring}
\title{Modify an existing keyring}
\usage{
modifyKeyring(keyring_id, secret = NULL)
}
\arguments{
\item{keyring_id}{A string specifying the keyring ID.}

\item{secret}{A secret string to be stored. It is not recommended to use this parameters because in that case your secret
will be visible in your code. This parameters should be used only for tests}
}
\value{
A successful message or an error message if keyring is not found.
}
\description{
This function modifies an existing keyring identified by the keyring ID.
If the keyring is not found, it shows a warning.
Notice: the keyringid is postfixed with _epiufkeyring to avoid conflicts
}
