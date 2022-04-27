# STRAP-epiuf
Utility function package  for STRAP  
  
can be installed as a library with :
devtools::install_github("Epiconcept-Paris/STRAP-epiuf")

WARNING : current epiuf package use xlsx which use java, then, Java must be installed before installing epiuf
Java version (i.e. 64-bit Java or 32-bit Java) must fits to the type of R version that you are using 
(use R.Version() to see which R version you are using )
To install Java 64-bit, go to the manual installation page: https://java.com/en/download/manual.jsp

For Mac M1 arm64 with R 4.2 ARM64,  version of Java is needed(see https://adoptium.net/).
Try /usr/libexec/java_home -V   
To list your Java Machine

if you have more than one version of R, you can install epiuf for the current version only using:
devtools::install_github("Epiconcept-Paris/STRAP-epiuf", INSTALL_opts=c("--no-multiarch"))


Once installed, you can use the library() function to load the package
library(epiuf)

**library(epiuf)** should replace the line  <  **source("epifunctions.R")**  >

