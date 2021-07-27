#setwd("/Users/dominiklawetzky/Documents/GitHub/sonntagsfrage/App")
#rm(list = ls())


## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "knitr", "shiny")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))
