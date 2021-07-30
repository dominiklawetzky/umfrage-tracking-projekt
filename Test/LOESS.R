# INTERVALL-ANFANG DEFINIEREN
min <-as.Date("01.01.15", format = "%d.%m.%y")

# INTERVALL-ENDE DEFINIEREN
max <- as.Date("01.01.20", format = "%d.%m.%y")

span <- 10

##### LOESS ----

# Union

Union_Allensbach <- dataset %>% filter(Partei == "Union" & Institut == "Allensbach")
Union_Infratest <- dataset %>% filter(Partei == "Union" & Institut == "Infratest")
Union_Forschungsgruppe <- dataset %>% filter(Partei == "Union" & Institut == "Forschungsgruppe Wahlen")

Union_Allensbach <- ksmooth(Union_Allensbach$Datum, Union_Allensbach$Prozent, kernel = "normal", bandwidth = 20)
Union_Infratest <- ksmooth(Union_Infratest$Datum, Union_Infratest$Prozent, kernel = "normal", bandwidth = 20)
Union_Forschungsgruppe <- ksmooth(Union_Forschungsgruppe$Datum, Union_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 20)

# SPD

SPD_Allensbach <- dataset %>% filter(Partei == "SPD" & Institut == "Allensbach")
SPD_Infratest <- dataset %>% filter(Partei == "SPD" & Institut == "Infratest")
SPD_Forschungsgruppe <- dataset %>% filter(Partei == "SPD" & Institut == "Forschungsgruppe Wahlen")

SPD_Allensbach <- ksmooth(SPD_Allensbach$Datum, SPD_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
SPD_Infratest <- ksmooth(SPD_Infratest$Datum, SPD_Infratest$Prozent, kernel = "normal", bandwidth = 10)
SPD_Forschungsgruppe <- ksmooth(SPD_Forschungsgruppe$Datum, SPD_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# Grüne

Grüne_Allensbach <- dataset %>% filter(Partei == "Grüne" & Institut == "Allensbach")
Grüne_Infratest <- dataset %>% filter(Partei == "Grüne" & Institut == "Infratest")
Grüne_Forschungsgruppe <- dataset %>% filter(Partei == "Grüne" & Institut == "Forschungsgruppe Wahlen")

Grüne_Allensbach <- ksmooth(Grüne_Allensbach$Datum, Grüne_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
Grüne_Infratest <- ksmooth(Grüne_Infratest$Datum, Grüne_Infratest$Prozent, kernel = "normal", bandwidth = 10)
Grüne_Forschungsgruppe <- ksmooth(Grüne_Forschungsgruppe$Datum, Grüne_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# Linke

Linke_Allensbach <- dataset %>% filter(Partei == "Linke*" & Institut == "Allensbach")
Linke_Infratest <- dataset %>% filter(Partei == "Linke*" & Institut == "Infratest")
Linke_Forschungsgruppe <- dataset %>% filter(Partei == "Linke*" & Institut == "Forschungsgruppe Wahlen")

Linke_Allensbach <- ksmooth(Linke*_Allensbach$Datum, Linke*_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
Linke_Infratest <- ksmooth(Linke*_Infratest$Datum, Linke*_Infratest$Prozent, kernel = "normal", bandwidth = 10)
Linke_Forschungsgruppe <- ksmooth(Linke*_Forschungsgruppe$Datum, Linke*_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# FDP

FDP_Allensbach <- dataset %>% filter(Partei == "FDP" & Institut == "Allensbach")
FDP_Infratest <- dataset %>% filter(Partei == "FDP" & Institut == "Infratest")
FDP_Forschungsgruppe <- dataset %>% filter(Partei == "FDP" & Institut == "Forschungsgruppe Wahlen")

FDP_Allensbach <- ksmooth(FDP_Allensbach$Datum, FDP_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
FDP_Infratest <- ksmooth(FDP_Infratest$Datum, FDP_Infratest$Prozent, kernel = "normal", bandwidth = 10)
FDP_Forschungsgruppe <- ksmooth(FDP_Forschungsgruppe$Datum, FDP_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# AfD

AfD_Allensbach <- dataset %>% filter(Partei == "AfD" & Institut == "Allensbach")
AfD_Infratest <- dataset %>% filter(Partei == "AfD" & Institut == "Infratest")
AfD_Forschungsgruppe <- dataset %>% filter(Partei == "AfD" & Institut == "Forschungsgruppe Wahlen")

AfD_Allensbach <- ksmooth(AfD_Allensbach$Datum, AfD_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
AfD_Infratest <- ksmooth(AfD_Infratest$Datum, AfD_Infratest$Prozent, kernel = "normal", bandwidth = 10)
AfD_Forschungsgruppe <- ksmooth(AfD_Forschungsgruppe$Datum, AfD_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# Piraten

Piraten_Allensbach <- dataset %>% filter(Partei == "Piraten" & Institut == "Allensbach")
Piraten_Infratest <- dataset %>% filter(Partei == "Piraten" & Institut == "Infratest")
Piraten_Forschungsgruppe <- dataset %>% filter(Partei == "Piraten" & Institut == "Forschungsgruppe Wahlen")

Piraten_Allensbach <- ksmooth(Piraten_Allensbach$Datum, Piraten_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
Piraten_Infratest <- ksmooth(Piraten_Infratest$Datum, Piraten_Infratest$Prozent, kernel = "normal", bandwidth = 10)
Piraten_Forschungsgruppe <- ksmooth(Piraten_Forschungsgruppe$Datum, Piraten_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)

# Andere

Andere_Allensbach <- dataset %>% filter(Partei == "Andere" & Institut == "Allensbach")
Andere_Infratest <- dataset %>% filter(Partei == "Andere" & Institut == "Infratest")
Andere_Forschungsgruppe <- dataset %>% filter(Partei == "Andere" & Institut == "Forschungsgruppe Wahlen")

Andere_Allensbach <- ksmooth(Andere_Allensbach$Datum, Andere_Allensbach$Prozent, kernel = "normal", bandwidth = 10)
Andere_Infratest <- ksmooth(Andere_Infratest$Datum, Andere_Infratest$Prozent, kernel = "normal", bandwidth = 10)
Andere_Forschungsgruppe <- ksmooth(Andere_Forschungsgruppe$Datum, Andere_Forschungsgruppe$Prozent, kernel = "normal", bandwidth = 10)


##### PLOT -----

  plot_ly() %>%
  add_lines(x=Union_Allensbach$x, y=Union_Allensbach$y, line=line.fmt) %>%
  layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
         yaxis = list(title = "Prozent (geglaettet)"),
         margin = c(1,1,1,1),
         annotations = 
           list(x = 1, y = -0.2, text = sprintf("Quelle: %s", test), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="grey"))) %>%
  layout(annotations = 
           list(x = .6 , y = 1.175, text = "Local Polynomial Regression Fitting", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=14, color="grey")))#



plot2_loess <- dataset %>% filter(between(Datum, min, max)) %>%
  plot_ly(x = ~Datum, y = ~Prozent) %>%
  add_lines(x=Union_Allensbach$x, y=Union_Allensbach$y, name = "Union (Allensbach)") %>%
  add_lines(x=Grüne_Allensbach$x, y=Grüne_Allensbach$y) %>%
  layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
         yaxis = list(title = "Prozent (geglaettet)"),
         margin = c(1,1,1,1),
         annotations = 
           list(x = 1, y = -0.2, text = sprintf("Quelle: %s", test), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="grey"))) %>%
  layout(annotations = 
           list(x = .6 , y = 1.175, text = "Local Polynomial Regression Fitting", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=14, color="grey")))
plot2_loess
