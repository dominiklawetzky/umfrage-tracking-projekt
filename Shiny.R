source("Preamble.R", local = TRUE)
source("Data.R", local = TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)


##### UI ------


ui <- fluidPage(theme = shinythemes::shinytheme("simplex"),
                titlePanel("Umfrage-Tracking-Projekt"),
                sidebarLayout(
                  
                  
                  sidebarPanel(
                    
                    # INSTITUT AUSWÄHLEN
                    # 
                    # selectInput(inputId = "institut", label = strong("Umfrage-Institut"),
                    #             choices = unique(dataset$Institut),
                    #             selected = "Travel"),
                    
                    checkboxGroupInput(inputId = "institut", label = strong("Umfrage-Institut"),
                                       selected = unique(dataset$Institut),
                                       choices = unique(dataset$Institut)),
                    
                    # ZEITINTERVALL AUSWÄHLEN
                    
                    dateRangeInput("date", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-07-20",
                                   min = "2010-01-01", max = "2021-07-20",
                                   language = "de", separator = "bis"),
                    
                    
                    # LOESS-GLÄTTUNG JA/NEIN
                    checkboxInput(inputId = "smoother", label = strong("LOESS-Glättung aktivieren"), value = FALSE),
                    
                    
                    # SPAN DER LOESS-GLÄTTUNG
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.1, max = 5, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness."))

                  ),
                  
        
                  # OUTPUT-PANEL
                  
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                
                                # TAB 1
                                tabPanel("Infektionszahlen", 
                                         
                                         plotOutput("plot1",
                                                    width = "100%"),
                                         downloadButton("plot1_download_pdf", label = "Als PDF herunterladen")
                                         ),
                                
                                # TAB 2
                                tabPanel("Altersstruktur", 
                                         plotOutput("age",
                                                    width = "100%"),
                                         
                                         br(),
                                         
                                         plotOutput("age_line"),
                                         
                                         br(),
                                         
                                         plotOutput("age_line2")),
                                
                                # TAB 3
                                tabPanel("Todeszahlen", 
                                         plotOutput("deaths",
                                                    width = "100%")),
                                
                                # TAB 4
                                tabPanel("Info",
                                         h2("Plots herunterladen"),
                                         p("Die Plots können über einen Rechtsklick mittels \"Speichern unter\" und ggf. unter Hinzufügen des Suffix \".png\" gespeichert werden."),
                                         h2("Glättungsmethoden"),
                                         HTML("<p>lm: linear regression model <br> glm: generalized linear model <br> gam: generalized additive mode <br> loess: locally estimated scatterplot smoothing</p>"))                           
                                
                               )
                    
                            )
                )
                                      
               )


##### Server Logic ------
server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    
    # ZEITINTERVALL FESTLEGEN
    
    min <- as.Date(input$date[1], format = "%d.%m.%y")
    max <- as.Date(input$date[2], format = "%d.%m.%y")
  
  
  # FARBEN FESTLEGEN
  
   colors <- c("#019ee3", "#7c7c7c", "#fbeb04", "#1ca42c", "#bd3076", "#f39200", "#e2001a", "#000000")
   
   
   colors_alt <- c("AfD" = "#019ee3", 
                   "Andere" = "#7c7c7c", 
                   "FDP" = "#fbeb04", 
                   "Grüne" = "#1ca42c", 
                   "Linke*" = "#bd3076", 
                   "Piraten" = "#f39200",
                   "SPD" = "#e2001a", 
                   "Union" = "#000000")
   
   # DATEN FILTERN
   
   filtered_dataset <- reactive({
     dplyr::filter(dataset, Institut == input$institut)
   })
                   
                   # PLOT 1
                   
                   plot1 <- ggplot(data = filtered_dataset()) +
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
                                   y = max(na.omit(filtered_dataset()$Prozent))*0.95,
                                   label = "Ergebnis",
                                   hjust = -.225,
                                   vjust = 0,
                                   angle = 0,
                                   fontface = 2), 
                               color = "black",
                               size = 4) +
                     geom_text(data = wahl, 
                               aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                   y = max(na.omit(filtered_dataset()$Prozent))*0.875,
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
                   
                   
                   
                   plot1_gl <- ggplot(data = filtered_dataset()) +
                     geom_vline(xintercept = as.Date(wahl$Datum, format = "%d.%m.%y"), 
                                color = "darkgrey", 
                                linetype = "dashed") +
                     geom_text(data = wahl, 
                               aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                   y = max(na.omit(filtered_dataset()$Prozent))*0.95,
                                   label = "Ergebnis",
                                   hjust = -.225,
                                   vjust = 0,
                                   angle = 0,
                                   fontface = 2), 
                               color = "black",
                               size = 4) +
                     geom_text(data = wahl, 
                               aes(x = as.Date(Datum, format = "%d.%m.%y"),
                                   y = max(na.omit(filtered_dataset()$Prozent))*0.875,
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
                     geom_smooth(aes(x = as.Date(Datum, format = "%d.%m.%y"), 
                                     y = Prozent, 
                                     color = as.factor(Partei), 
                                     shape = as.factor(Institut)),
                                 method = "loess", se = TRUE, span = input$f) +
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
                   
                   ggsave("plot.pdf", plot1)
                   
                   if(input$smoother == 1){print(plot1_gl)}
                   else {print(plot1)}
                   
    
  })
  
  

  output$plot1_download <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      file.copy("plot.pdf", file, overwrite=TRUE)
    }
  )
  

}




##### Applikation starten -----
shinyApp(ui = ui, server = server)


