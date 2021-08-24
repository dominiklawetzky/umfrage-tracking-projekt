colors <-  c("AfD" = "#019ee3", 
    "Andere" = "#7c7c7c", 
    "FDP" = "#fbeb04", 
    "Freie Wähler" = "#084e8b",
    "Grüne" = "#1ca42c", 
    "Linke*" = "#bd3076", 
    "Piraten" = "#f39200",
    "SPD" = "#e2001a", 
    "Union" = "#000000")

dataset$Datum %>% max()

order <- list(categoryorder = "array",
              categoryarray = c("Union", 
                                "SPD", 
                                "AfD",
                                "FDP",
                                "Linke*",
                                "Grüne",
                                "Piraten",
                                "Andere"))

barplot <- dataset %>% subset(Institut == "Allensbach") %>% subset(Datum == "2021-08-17") %>% 
  plot_ly(x = ~Partei, y = ~Prozent, type = "bar", color = ~Partei, colors = colors) %>%
  layout(legend = list(font = list(size = 12)),
         xaxis = order,
         dragmode = "select",
         autosize = TRUE) %>%
  config(locale = "de") %>%
  layout(title = sprintf("Zustimmungswerte der großen politischen Parteien vom %s", format(as.Date("2021-08-17"), "%d.%m.%y")),
         margin = c(1,1,1,1)) # Plot 1 MIT Bundestagswahlen


barplot

dataset %>% filter(Institut %in% "Infratest") %>% # Filter nach Institut
  filter(Datum %in% max(Datum))

max(na.omit(allensbach_long$Prozent))
