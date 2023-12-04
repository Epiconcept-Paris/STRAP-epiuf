#' Dataset correspondence table between country/study site codes, country/study site names
#'
#' Dataframe providing the correspondence table of the id code \code{ID}
#' and the geographical label \code{Label} to use
#' throughout reports. Additional information is available in columns 
#' \code{LocationName},
#' \code{LocationType},
#' and \code{CountryISO2Code}
#' Original Excel spreadsheet \code{suggestion1_RefLocation.xlsx} available on GoogleDrive.
#'
#' @format A data frame with 40 rows and 5 variables:
#' \describe{
#'   \item{ID}{Code associated to a location (country or study site), e.g. AT, BE, BG, etc.}
#'   \item{Label}{Full name of the country or study site including 
#'   'the' article for NL and UK  e.g. Austria, Belgium, the Netherlands, the United Kingdom etc.}
#'   \item{LocationName}{Shorter name of the country or study site}
#'   \item{LocationType}{For location, variable specifying the goegraphical level, i.e., country or Nuts3 location}
#'   \item{CountryISO2Code}{ISO2 code of the corresponding country, e.g. ES for Navarra}
#' }
#' 
"RefSite"

#' Dummy dataset
#'
#' Dataframe with commun epidemiological variables to use for tests
#'
#' @format A data frame with 100 observations on 8 variables.
#' \describe{
#'   \item{id}{Identifier}
#'   \item{country}{Country (Albania,Croatia,Italy,Poland)}
#'   \item{CountryCode}{Code associated to a location}
#'   \item{CountryId}{CountryISO2Code}
#'   \item{age}{Age}
#'   \item{EnrolmentDate}{Date of enrolment in the study}
#'   \item{CovVaccBr}{Brand of the Covid vaccine (Pfizer, Moderna, Astra Zeneca)}
#'   \item{VaccDate}{date of the Covid vaccine}
#'   \item{case}{If infection (0=no, 1=yes)}
#'   \item{DatePos}{Date of infection}
#' }
#' 
"DummyData"
