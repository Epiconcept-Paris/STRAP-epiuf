
.onAttach <- function(libname, pkgname) {
  # nothing
}

.onLoad <- function(libname, pkgname) {
  ver <- utils::packageVersion("epiuf")
  datver <- as.character(utils::packageDate("epiuf"))
  # removed because of CRAN Notice (even if packageStartupMessage is the recommended method )
  # packageStartupMeassage generate a NOTE where it should not...
  # packageStartupMessage("Package epiuf Version",as.character(ver[[1]]),"(",datver,") loaded")
}