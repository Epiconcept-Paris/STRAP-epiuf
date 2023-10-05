#' epidata
#'
#' A data frame with 100 observations on 8 variables.
#'
#' @format data.frame
#' 
#' \describe{
#'   \item{id}{Identifier}
#'   \item{country}{Country (France, Spain or Georgia)}
#'   \item{age}{Age}
#'   \item{enrolmentdate}{Date of enrolment in the study}
#'   \item{covvaccbr}{Brand of the Covid vaccine (Pfizer, Moderna, Astra Zeneca)}
#'   \item{vaccdate}{date of the Covid vaccine}
#'   \item{case}{If infection (0=no, 1=yes)}
#'   \item{datepos}{Date of infection}
#' }
#'
#'
#' @examples
#' data(epidata)
#' head(epidata)
#'
#' @docType data
#' @name epidata
#' @family STRAP
#'
#' @return
#' data.frame with the dataset
#'
#' @author
#' epiteam
#'


# Set seed for reproducibility
set.seed(123)
prob_na <- 0.2 # probability of NA in a vector

# Generate a dataset with 100 observations
n <- 100

# Generate ID variable
id <- 1:n

# Generate character variable (Country)
country <- sample(c("France", "Spain", "Georgia"), n, replace = TRUE)

# Generate numeric variable (Age)
age <- sample(18:80, n, replace = TRUE)

# Generate factor variable (Covid Brand)
covvaccbr <- sample(c("Pfizer", "Moderna", "Astra Zeneca"), n, replace = TRUE)

# Generate binary variable (Covid Case)
case <- sample(0:1, n, replace = TRUE,prob = c(0.75,0.25))

# Date of infection 
datepos <- as.Date("2021-03-01") + sample((0:365)*2, n, replace = TRUE)
datepos[which(case==0)] <- NA

# Date of enrolment in the study
enrolmentdate <- as.Date("2021-05-01") + sample(0:365, n, replace = TRUE)
# Generate date variable (Vaccination Date)
vaccdate <- as.Date("2021-01-01") + sample(0:(365*2), n, replace = TRUE)
vaccdate <- as.Date(ifelse(runif(n) < prob_na, NA, vaccdate),origin = "1970-01-01")
covvaccbr[which(is.na(vaccdate))] <- NA

# Create the dataset
epidata <- data.frame(
            id = id,
            country = as.factor(country),
            age = age,
            enrolmentdate = enrolmentdate,
            covvaccbr = as.factor(covvaccbr),
            vaccdate = vaccdate,
            case = case,
            datepos = datepos
          )

# Print the first few rows of the dataset
head(epidata)

saveRDS(object = epidata,file = )


