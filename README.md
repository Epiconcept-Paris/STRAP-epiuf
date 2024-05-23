# STRAP-epiuf v0.5.1.1
Utility function package  for STRAP  
  
can be installed current code as a library with :
devtools::install_github("Epiconcept-Paris/STRAP-epiuf")

if you have more than one version of R, you can install epiuf for the current R version only using:
devtools::install_github("Epiconcept-Paris/STRAP-epiuf", INSTALL_opts=c("--no-multiarch"))

To install a specific version : 
devtools::install_github("Epiconcept-Paris/STRAP-epiuf@v0.4.0.0",upgrade="never",build_vignettes = TRUE,INSTALL_opts=c("--no-multiarch"))

To install the latest release : 
devtools::install_github("Epiconcept-Paris/STRAP-epiuf@*release",upgrade="never",build_vignettes = TRUE,INSTALL_opts=c("--no-multiarch"))

Once installed, you can use the library() function to load the package
library(epiuf)

**library(epiuf)** should replace the line  <  **source("epifunctions.R")**  >

