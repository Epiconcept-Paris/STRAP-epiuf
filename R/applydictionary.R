#
# Project Name : 
# Script Name  :
# GitHub repo  : 
# Summary      : 
# Date created : 
# Author       : 
# Date reviewed:
# Reviewed by  :

# Description --------------------------------------------------------------
# 
# 
# 
# 
# 


# Changes Log --------------------------------------------------------------
# 

# START of SCRIPT  --------------------------------------------------------



dicGenericName <- "generic_name"
dicSourceName <- "source_name"

#' applyDictionary
#'
#' @param dic A dictionary (passed as dataframe)
#' @param cur  A dataset to transform to generic
#' @param verbose Should we have feedback 
#' @param keepextra if TRUE, extra variables are keept in generic dataset (no longer generic then...)
#'
#' @return A data set 
#' @export
#'

applyDictionary <- function(dic, cur, verbose=TRUE, keepextra = FALSE) {
  NewNames <- unlist(dic[!is.na(dic[,dicGenericName]),dicGenericName])
  OldNames <- unlist(dic[ ! (dic[,dicSourceName]=="" ) ,dicSourceName])
  OldNames <- OldNames[! is.na(OldNames)]
  
  CurNames <- unlist(names(cur))
  # variables defined in source_name but missing in the imported dataset comparer to dico (dico not up to date)
  VarMiss <- setdiff(OldNames,CurNames)
  
  # Extra variables in source that are not defined in dico source_name
  VarExtra  <- setdiff(CurNames,OldNames)
  
  if (verbose) {
    epiuf::bold("Vars missing in imported : ", length(VarMiss))
    catret()
    catret(sort(VarMiss),sep="  \n")
  }  
  if  (length(VarExtra) >0 ) {
    if (! keepextra ) {
      # we remove the extra column except if exists in generic 
      epiuf::bold("Extra vars in imported (dropped if not exists in generic) : ",length(VarExtra))
      catret()
      catret(sort(VarExtra),sep="  \n")
      gen <- cur[, -which( (names(cur) %in% VarExtra) & !(names(cur)%in%NewNames)  )]
    } else {
      epiuf::bold("Extra vars in imported keept in generic  : ",length(VarExtra))
      catret()
      catret(sort(VarExtra),sep="  \n")
      gen <- cur  
    }
  } else { gen <- cur } 
  
  # we merge only the matching
  CurNames <- as.data.frame(CurNames)
  MatchNames <-  merge(dic,CurNames, by.x=dicSourceName, by.y="CurNames")
  todrop <- MatchNames[is.na(MatchNames[dicGenericName]),dicSourceName]
  if (verbose) {
    epiuf::bold("Vars not in generic and dropped  : ", length(todrop))
    catret()
    catret(sort(todrop),sep="  \n")
  } 
  
  gen <- gen[, -which( (names(gen) %in% todrop) )]
  
  
  MatchNames <- MatchNames[! is.na(MatchNames[dicGenericName]),]
  
  if (verbose) {
    bold("Imported vars renamed with a generic name : ", nrow(MatchNames))
    catret()
    listMatchNames <- list()
    for (i in 1:nrow(MatchNames) ){
      listMatchNames[i] <- paste(MatchNames[i,dicGenericName],"<=",MatchNames[i,dicSourceName])
    }
    catret(unlist(listMatchNames),sep="\n")
    if (nrow(MatchNames) > 20) {
      catret("...")
    }  
  }
  # rename the existing
  for (i in 1:nrow(MatchNames)) {
    thenewname <- MatchNames[i,dicGenericName]
    theoldname <- MatchNames[i,dicSourceName]
    names(gen)[names(gen) == theoldname] <- thenewname 
  }
  
  #now check for missing in final
  CurNames <- names(gen)
  VarMiss <- setdiff(NewNames,CurNames)
  for (i in 1:length(VarMiss) ) {
    typevar <-  dic[dic[[dicGenericName]]==VarMiss[i],"type"]
    typevar <- typevar[! is.na(typevar)]
    valuevar <- switch (typevar,
                        "numeric" = 0,
                        "character" = "",
                        "date" = date(),
                        ""
    )
    gen[,VarMiss[i]] <- valuevar
  } 
  if (verbose) {
    bold("Generic vars created (as empty) : ", length(VarMiss))
    catret()
    catret(sort(VarMiss), sep=", ")
  }
  
  gen
}




# END of SCRIPT  --------------------------------------------------------