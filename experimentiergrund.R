library(lubridate)
library(plotly)

plot_ly(dataset, x = ~Datum, y = ~Prozent) %>%
  add_lines(linetype = ~Institut, color = ~ordered(Partei))


?add_lines

plot <- dataset %>% filter(Institut == "Infratest") %>%
           plot_ly(x = ~Datum, y = ~Prozent) %>%
           add_lines(color = ~Partei, colors = colors_alt)
  

dataset$Datum <- as.Date(dataset$Datum, format = "%d.%m.%y")

dataset %>% filter(between(Datum, as.Date(min), as.Date(max))) %>%
  filter(Institut == institut)
      plot_ly(x = ~Datum, y = ~Prozent) %>%
      add_lines(color = ~Partei, 
                colors = colors_alt,
                linetype = ~Institut) %>%
      layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
             # legend = list(orientation = "h",   
             #               xanchor = "center",  
             #               x = .5,
             #               y = -.25),
             margin = c(10,10,10,10))

plot


my_loess <- function(data, span){
  loess(Prozent ~ Datum,
        data = data,
        na.action = na.exclude,
        span = span)
}


loess()

dat <- dataset %>% filter(Institut %in% institut) %>%
           filter(Partei %in% partei)

dat$ID <- 1:nrow(dat)

dat$smoothed <- ksmooth(dat$ID, dat$Prozent, kernel = "normal", bandwidth = .5)$y



?ksmooth

loess(Prozent ~ Datum, data = dat, span = .5, na.action = na.exclude)
ksmooth(Datum, Prozent, kernel = "normal", bandwidth = .5)

lm(Prozent ~ Datum*Institut,
   data = dataset)
?loess

ksmooth(Datum, Prozent)

?loess.smooth
institut <- c("Forschungsgruppe Wahlen")
partei <- c("Union")

dataset %>% filter(Institut %in% institut) %>%
  summary()


dataset %>%
  summary()

dataset %>% filter(Institut == institut) %>%
  summary()

filtered_data

filtered_data %>% filter(between(Datum, as.Date(min, as.Date(max)) %>%
  plot_ly(x = ~Datum, y = ~Prozent) %>%
  add_lines(color = ~Partei, 
            colors = colors,
            linetype = ~Institut) %>%
  layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
         # legend = list(orientation = "h",   
         #               xanchor = "center",  
         #               x = .5,
         #               y = -.25),
         margin = c(1,1,1,1))
  
plot1 




        
plot %>% 
  add_fun(function(plot) {
    plot %>% layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
                    xaxis = list(tickformat = "%m.%y",
                                 range=c(min, max),
                                 type = "date"),
                    margin = c(1,1,1,1,4))
  })
  
  layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
    xaxis = list(tickformat = "%m.%y",
                 range=c(min, max),
                 type = "date"),
    margin = c(1,1,1,1))




fig <- fig %>% add_trace(y = ~Prozent, name = "Forschungsgruppe Wahlen", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~Prozent, name = "Infratest", mode = 'lines+markers') 

fig


data(economics, package = "ggplot2")
mode = "markers+lines+text",


min <- format(as.Date("07.01.20", format = "%d.%m.%y"), "%Y-%m-%d")
max <- format(as.Date("17.07.21", format = "%d.%m.%y"), "%Y-%m-%d")

min <- as.Date("07.01.10", format = "%d.%m.%y")
max <- as.Date("17.07.21", format = "%d.%m.%y")

plot1 <- ggplot(dataset) +
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
                y = max(na.omit(dataset$Prozent))*0.95,
                label = "Ergebnis",
                hjust = -.225,
                vjust = 0,
                angle = 0,
                fontface = 2), 
            color = "black",
            size = 4) +
  geom_text(data = wahl, 
            aes(x = as.Date(Datum, format = "%d.%m.%y"),
                y = max(na.omit(dataset$Prozent))*0.875,
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
  labs(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min, "%d.%m.%y")),
       subtitle = "Stand: 17. Juli 2021",
       color = "Parteien",
       linetype = "Institute",
       shape = "Institute",
       x = "Datum",
       caption = "github.com/dominiklawetzky/sonntagsfrage") +
  scale_color_manual(name = "Parteien", values = colors_alt) +
  #theme_fivethirtyeight() +
  theme_light() +
  theme(axis.text.x=element_text(size=rel(.75), 
                                 angle=90, 
                                 margin = margin(b = 12)),
        plot.title = element_text(size = 18, 
                                  face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position="bottom") +
  scale_x_date(breaks = "6 month", 
               limits = c(min, max))


ggplotly(plot1)
