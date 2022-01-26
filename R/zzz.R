
.onAttach <- function(libname, pkgname) {
  # nothing
}

.onLoad <- function(libname, pkgname) {
  ver <- packageVersion("epiuf")
  catret("Package epiuf Version",as.character(ver[[1]]), "loaded")
}