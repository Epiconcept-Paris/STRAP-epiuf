
.onAttach <- function(libname, pkgname) {
  # nothing
}

.onLoad <- function(libname, pkgname) {
  ver <- packageVersion("epiuf")
  datver <- as.character(packageDate("epiuf"))
  catret("Package epiuf Version",as.character(ver[[1]]),"(",datver,") loaded")
}