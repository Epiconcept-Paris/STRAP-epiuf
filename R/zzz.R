
.onAttach <- function(libname, pkgname) {
  # nothing
}

.onLoad <- function(libname, pkgname) {
  ver <- utils::packageVersion("epiuf")
  datver <- as.character(utils::packageDate("epiuf"))
  # packageStartupMeassage generate a NOTE where it should not...
  # packageStartupMessage("Package epiuf Version",as.character(ver[[1]]),"(",datver,") loaded")
}