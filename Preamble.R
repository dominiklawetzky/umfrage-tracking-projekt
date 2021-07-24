##### Preamble -----

rm(list = ls())

# Working Directory
setwd("/Users/dominiklawetzky/Documents/GitHub/sonntagsfrage")

## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma", "emmeans", "ez", "ggthemes")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))
