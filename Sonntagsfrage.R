##### Preamble -----

rm(list = ls())

# Working Directory
setwd("/Users/dominiklawetzky/Documents/GitHub/sonntagsfrage")

## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma", "emmeans", "ez")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))


##### Infratest laden -----

source("Daten/infratest.R") # Infratest-Daten laden

infratest$AfD <- round(as.numeric(infratest$AfD), 0)
infratest$FDP <- round(as.numeric(infratest$FDP), 0)
infratest$Piraten <- round(as.numeric(infratest$Piraten), 0)
infratest$Datum <- format(as.Date(infratest$Datum, format="%d.%m.%y"), "%d.%m.%y")

infratest_long <- infratest %>%
                      pivot_longer(!Datum, names_to = "Partei", values_to = "Prozent") # In Longformat umwandeln

infratest_long$Partei <- as.factor(infratest_long$Partei)
infratest_long$Datum <- format(as.Date(infratest_long$Datum, format="%d.%m.%y"), "%d.%m.%y")

infratest_long$Institut <- c("Infratest")


##### Allensbach laden -----

allensbach <- read.csv("Daten/allensbach.csv", header = TRUE, sep = c(";", " - "), dec = ",")

allensbach$Datum <- substr(allensbach$Zeitraum, 9, nchar(allensbach$Zeitraum)) # Nur Endzeitpunkt der Erhebung als Datum

allensbach$Datum <- format(as.Date(allensbach$Datum, format="%d.%m.%Y"), "%d.%m.%y") # Datumsformat

names(allensbach)[2] <- "Union"
names(allensbach)[5] <- "Grüne"
names(allensbach)[6] <- "Linke*"
names(allensbach)[7] <- "AfD"
names(allensbach)[8] <- "Andere"

allensbach <- subset(allensbach, select = -c(Zeitraum)) # Variable Zeitraum löschen


allensbach_long <- allensbach %>%
                        pivot_longer(!Datum, names_to = "Partei", values_to = "Prozent") # In Longformat umwandeln

allensbach_long$Institut <- c("Allensbach")


##### Forschungsgruppe Wahlen -----

forschungsgruppe <- read.csv("Daten/forschungsgruppe.csv", header = TRUE, sep = c(";"), dec = ".", na.strings = "NA")

forschungsgruppe <- subset(forschungsgruppe, select = -c(X, n, Zeitraum)) # Überflüssige Variablen löschen

names(forschungsgruppe)[6] <- "Linke*" 

forschungsgruppe$Piraten <- as.numeric(forschungsgruppe$Piraten)
forschungsgruppe$Union <- as.numeric(forschungsgruppe$Union)
forschungsgruppe$FDP <- as.numeric(forschungsgruppe$FDP)
forschungsgruppe$AfD <- as.numeric(forschungsgruppe$AfD)

forschungsgruppe$Datum <- format(as.Date(forschungsgruppe$Datum, format="%d.%m.%Y"), "%d.%m.%y") # Datumsformat

forschungsgruppe_long <- forschungsgruppe %>%
                             pivot_longer(!Datum, names_to = "Partei", values_to = "Prozent") # In Longformat umwandeln

forschungsgruppe_long$Institut <- c("Forschungsgruppe Wahlen")

forschungsgruppe_long$Prozent <- forschungsgruppe_long$Prozent*100

##### Datensätze aggregieren -----

dataset <- rbind(allensbach_long, infratest_long, forschungsgruppe_long)

dataset$Institut <- as.factor(dataset$Institut)


##### Wahlergebnisse -----

wahl <- data.frame(Datum = c("22.09.2013", "24.09.2017"),
                   Name = c("Bundestagwahl 2013", "Bundestagswahl 2017"),
                   Union = c(41.5, 32.9),
                   SPD = c(25.7, 20.5),
                   FDP = c(4.8, 10.7),
                   Grüne = c(8.4, 8.9),
                   Linke = c(8.6, 9.2),
                   AfD = c(NA, 12.6))

wahl$Datum <- format(as.Date(wahl$Datum, format="%d.%m.%Y"), "%d.%m.%y")

names(wahl)[7] <- "Linke*"

wahl_long <- subset(wahl, select = -c(Name)) %>%
               pivot_longer(!Datum, names_to = "Partei", values_to = "Prozent") # In Longformat umwandeln

wahl_long$Datum <-  format(as.Date(wahl_long$Datum, format="%d.%m.%Y"), "%d.%m.%y")

wahl_long

##### Deskriptivstatistik -----


colors <- c("#019ee3", "#7c7c7c", "#fbeb04", "#1ca42c", "#bd3076", "#f39200", "#e2001a", "#000000")
colors_alt <- c("#019ee3", "#7c7c7c", "#fbeb04", "#1ca42c", "#bd3076", "#e2001a", "#000000")

## PLOT 1 (10 Jahre / alle Institute / alle Parteien)

min <- as.Date("07.01.10", format = "%d.%m.%y")
max <- as.Date("01.07.21", format = "%d.%m.%y")

plot1 <- ggplot(data = dataset) +
                  geom_point(aes(x = as.Date(Datum, format = "%d.%m.%y"), 
                                 y = Prozent, 
                                 color = as.factor(Partei), 
                                 shape = as.factor(Institut)), 
                             stat = "identity", 
                             size = .8) +
                  geom_line(aes(x = as.Date(Datum, format = "%d.%m.%y"), 
                                y = Prozent, 
                                color = as.factor(Partei), 
                                linetype = as.factor(Institut))) +
                  geom_vline(xintercept = as.Date(wahl$Datum, format = "%d.%m.%y"), 
                             color = "darkgrey", 
                             linetype = "dashed") +
                  geom_text(data = wahl, 
                            aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                y = 49.5,
                                label = "Ergebnis",
                                hjust = -.225,
                                vjust = 0,
                                angle = 0,
                                fontface = 2), 
                            color = "black",
                            size = 4) +
                  geom_text(data = wahl, 
                            aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                y = 47.5,
                                label = Name,
                                hjust = -.1,
                                vjust = 0,
                                angle = 0), 
                            color = "black",
                            size = 4) +
                  geom_point(data = wahl_long,
                             aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                 y = Prozent,
                                 color = as.factor(Partei)),
                             size = 5,
                             shape = 15) +
                  # geom_text(data = wahl_long, 
                  #           aes(x = as.Date(Datum, format = "%d.%m.%y"),
                  #               y = Prozent,
                  #               label = Prozent,
                  #               hjust = -.1,
                  #               vjust = -1.5,
                  #               angle = 0), 
                  #           color = "black",
                  #           size = 1.5) +
                  # geom_smooth(method = "loess", se = TRUE, span = 2) +
                  labs(title = "Zustimmungswerte der großen politischen Parteien", 
                       subtitle = "seit Januar 2010 über verschiedene Umfrage-Institute",
                       color = "Parteien",
                       linetype = "Institute",
                       shape = "Institute",
                       x = "Datum",
                       caption = "github.com/dominiklawetzky/sonntagsfrage") +
                  scale_color_manual(name = "Parteien", values = colors) +
                  theme_light() +
                  theme(axis.text.x=element_text(size=rel(.75), 
                                                 angle=90, 
                                                 margin = margin(b = 12))) +
                  theme(plot.title = element_text(size = 18, 
                                                  face = "bold")) +
                  theme(legend.position="bottom") +
                  scale_x_date(breaks = "6 month", 
                               limits = c(min, max))


ggsave(file="plot1.jpg", plot=plot1, width=15, height=8)


## PLOT 2

min <- as.Date("09.01.20", "%d.%m.%y")
max <- as.Date("01.07.21", "%d.%m.%y")

plot2 <- ggplot(data = dataset, aes(x = as.Date(Datum, format = "%d.%m.%y"), y = Prozent, color = as.factor(Partei), linetype = as.factor(Institut), shape = as.factor(Institut))) +
                  geom_point(stat = "identity", size = .8) +
                  geom_line() +
                  #geom_smooth(method = "loess", se = TRUE, span = 2) +
                  labs(title = "Zustimmungswerte der großen politischen Parteien", 
                       subtitle = "seit Januar 2020 über verschiedene Umfrage-Institute",
                       color = "Parteien",
                       linetype = "Institute",
                       shape = "Institute",
                       x = "Datum",
                       caption = "github.com/dominiklawetzky/sonntagsfrage") +
                  scale_color_manual(name = "Parteien", values = colors) +
                  theme_light() +
                  theme(axis.text.x=element_text(size=rel(.75), angle=90, margin = margin(b = 12))) +
                  theme(plot.title = element_text(size = 18, face = "bold")) +
                  theme(legend.position="bottom") +
                  scale_x_date(breaks = "1 month", limits = c(min, max))

ggsave(file="plot2.jpg", plot=plot2, width=10, height=6)




## PLOT 3

min <- as.Date("09.01.20", "%d.%m.%y")
max <- as.Date("01.07.21", "%d.%m.%y")

plot3 <- ggplot(data = dataset, aes(x = as.Date(Datum, format = "%d.%m.%y"), y = Prozent, color = as.factor(Partei), linetype = as.factor(Institut), shape = as.factor(Institut))) +
                  geom_smooth(method = "loess", se = FALSE, span = .5) +
                  labs(title = "Zustimmungswerte der großen politischen Parteien", 
                       subtitle = "seit Januar 2020 über verschiedene Umfrage-Institute (LOESS-Glättung)",
                       color = "Parteien",
                       linetype = "Institute",
                       shape = "Institute",
                       x = "Datum",
                       caption = "github.com/dominiklawetzky/sonntagsfrage") +
                  scale_color_manual(name = "Parteien", values = colors) +
                  theme_light() +
                  theme(axis.text.x=element_text(size=rel(.75), angle=90, margin = margin(b = 12))) +
                  theme(plot.title = element_text(size = 18, face = "bold")) +
                  theme(legend.position="bottom") +
                  scale_x_date(breaks = "2 week", limits = c(min, max))

ggsave(file="plot3.jpg", plot=plot3, width=10, height=6)




## PLOT 4

min <- as.Date("09.01.20", "%d.%m.%y")
max <- as.Date("01.07.21", "%d.%m.%y")

plot4 <- ggplot(data = dataset, aes(x = as.Date(Datum, format = "%d.%m.%y"), y = Prozent, color = as.factor(Partei), linetype = as.factor(Institut), shape = as.factor(Institut))) +
                geom_smooth(method = "loess", se = TRUE, span = .5) +
                labs(title = "Zustimmungswerte der großen politischen Parteien", 
                     subtitle = "seit Januar 2020 über verschiedene Umfrage-Institute (LOESS-Glättung und Konfidenzintervall)",
                     color = "Parteien",
                     linetype = "Institute",
                     shape = "Institute",
                     x = "Datum",
                     caption = "github.com/dominiklawetzky/sonntagsfrage") +
                scale_color_manual(name = "Parteien", values = colors) +
                theme_light() +
                theme(axis.text.x=element_text(size=rel(.75), angle=90, margin = margin(b = 12))) +
                theme(plot.title = element_text(size = 18, face = "bold")) +
                theme(legend.position="bottom") +
                scale_x_date(breaks = "2 week", limits = c(min, max))

ggsave(file="plot4.jpg", plot=plot4, width=10, height=6)





## PLOT 5

min <- as.Date("07.01.10", "%d.%m.%y")
max <- as.Date("01.07.21", "%d.%m.%y")

plot5 <- ggplot(data = dataset, aes(x = as.Date(Datum, format = "%d.%m.%y"), y = Prozent, color = as.factor(Partei))) +
                geom_smooth(method = "loess", se = TRUE, span = .5) +
                labs(title = "Zustimmungswerte der großen politischen Parteien", 
                     subtitle = "seit Januar 2010 über verschiedene Umfrage-Institute (LOESS-Glättung und Konfidenzintervall)",
                     color = "Parteien",
                     x = "Datum",
                     caption = "github.com/dominiklawetzky/sonntagsfrage") +
                scale_color_manual(name = "Parteien", values = colors) +
                theme_light() +
                theme(axis.text.x=element_text(size=rel(.75), angle=90, margin = margin(b = 12))) +
                theme(plot.title = element_text(size = 18, face = "bold")) +
                theme(legend.position="bottom") +
                scale_x_date(breaks = "6 month", limits = c(min, max)) +
                facet_wrap(~Institut)

ggsave(file="plot5.jpg", plot=plot5, width=15, height=6)

