#
# Project Name : STRAP
# Script Name  : getDelayIf
# GitHub repo  : SARI-VEBIS-OLD
# Summary      : developement of getDelayIf function
# Date created : 16/02/2022
# Author       : JHD GDE
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# Function aims to retreive delay in dates between any two given dates, and 
# enables specifications of conditions that need to be met, otherwise NA given.
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------



#' Title
#'
#' @param data 
#' @param date1 
#' @param date2 
#' @param ... one or more logical condition to validate. If one of the condition is not TRUE, 
#'            NA is returned instead of the calculated delay 
#'
#' @return a vector of the same length than the passed dataset
#' @export
#'
#' @examples
#' \dontrun{
#'    delay(data ,date1,date2, date2 > date1 + 14)
#' }
#' 
getDelayIf <- function(data, date1, date2, ...) {

  # first we catch the ... parameters
  listcond <- as.list(match.call())
  # we remove the first one which is the function name
  listcond[1] <- NULL
  # and we remove all named parameters from the list
  namedarg <- pmatch(c("data","date1","date2"),names(listcond), nomatch = 0)
  if (length(namedarg) > 0 ) {listcond[namedarg] <- NULL }

  # We verify that parameters are language and if it is "test" then we parse as language
  for (i in 1:length(listcond)) {
    if (!typeof(listcond[i])=="language") {listcond[i] <- parse(text=listcond[i])}
  }
  
  # we get the named parameters as "language" with substitute
  date1 <- as.character(substitute(date1))
  date2 <- as.character(substitute(date2))

  # we add a delay variable to the dataset (should we ?) 
  data$temp__delay <- as.numeric(data[, date2] - data[, date1])
  
  # we loop over condition 
  for (i in 1:length(listcond)) {
    onecond = listcond[[1]]
    # we get index of row which meet the condition 
    meet <- which(with(data,eval(onecond)))
    # delay is NA if the condition is not meet
    data[ ! meet,"temp__delay"] <- NA
  }
  result <- data$temp__delay
  data$temp__delay <- NULL
  
  return(result)
}


# Tester code: ----
 df$test <- getDelayIf(df,CovvaccdateFirstdose, CovvaccdateSeconddose, 
                     CovvaccanyFirstdose==1, CovvaccanySeconddose==1)

 viewIf(df,CovvaccanyFirstdose,CovvaccdateFirstdose,test,nline = 15, cond=!is.na(CovvaccdateFirstdose))

# END of SCRIPT  --------------------------------------------------------

