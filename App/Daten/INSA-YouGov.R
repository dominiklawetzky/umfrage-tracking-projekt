
library(XML)
library(RCurl)
library(rlist)
library(readr)

# Daten crawlen
theurl <- getURL("https://www.wahlrecht.de/umfragen/insa.htm",.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)

# Dataframe bilden
df <- data.frame(tables, stringsAsFactors = TRUE)

# Variablen sortieren
colnames(df) <- c("Datum", "Empty", "Union", "SPD", "Grüne", "FDP", "Linke*", "Piraten", "FW", "AfD", "Andere", "Empty", "Befragte", "Zeitraum")

drops <- c("Empty","Befragte", "Zeitraum")
df <- df[ , !(names(df) %in% drops)]
df <- df %>% filter(Datum != "02.02.2017") # Fehlerhafte Zeile löschen

# Prozente zu Dezimalzahl umwandeln
df$Union <- parse_number(df$Union, locale = locale(decimal_mark = ','))
df$SPD <- parse_number(df$SPD, locale = locale(decimal_mark = ','))
df$Grüne <- parse_number(df$Grüne, locale = locale(decimal_mark = ','))
df$FDP <- parse_number(df$FDP, locale = locale(decimal_mark = ','))
df$"Linke*" <- parse_number(df$"Linke*", locale = locale(decimal_mark = ','))
df$Piraten <- parse_number(df$Piraten, locale = locale(decimal_mark = ','))
df$FW <- parse_number(df$FW, locale = locale(decimal_mark = ','))
df$AfD <- parse_number(df$AfD, locale = locale(decimal_mark = ','))
df$Andere <- parse_number(df$Andere, locale = locale(decimal_mark = ','))

# Datum formatieren
df$Datum <- format(as.Date(df$Datum, format = "%d.%m.%Y"), "%d.%m.%y")

# Variablen umbenennen
names(df)[8] <- "Freie Wähler"

# Datensatz erstellen
INSA_YouGov <- df