source("Preamble.R", local = TRUE)
source("Data.R", local = TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(plotly)


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
                                       selected = c("Allensbach", "Forschungsgruppe Wahlen", "Infratest"),
                                       choices = c("Allensbach", "Forschungsgruppe Wahlen", "Infratest")),
                    
                    # ZEITINTERVALL AUSWÄHLEN
                    
                    dateRangeInput("date", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-07-20",
                                   min = "2010-01-01", max = "2021-07-20",
                                   format = "dd.mm.yyyy", language = "de", separator = "bis"),

                    
                    # LOESS-GLÄTTUNG JA/NEIN
                    checkboxInput(inputId = "smoother", label = strong("LOESS-Glättung aktivieren"), value = FALSE),
                    
                    
                    # SPAN DER LOESS-GLÄTTUNG
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.1, max = 5, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Je größer der Wert, desto stärker die Glättung."))

                  ),
                  
        
                  # OUTPUT-PANEL
                  
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                
                                # TAB 1
                                tabPanel("Übersicht", 
                                         
                                         plotlyOutput("plot1",
                                                      height = "auto", width = "auto"),
                                         downloadButton("plot1_download_pdf", label = "Als PDF herunterladen")
                                         ),
                                
                                # TAB 2
                                tabPanel("Trendanalyse", 
                                         plotOutput("age",
                                                    width = "100%"),
                                         
                                         br(),
                                         
                                         plotOutput("age_line"),
                                         
                                         br(),
                                         
                                         plotOutput("age_line2")),
                                
                                # TAB 3
                                tabPanel("...", 
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
  
  # INTERVALL-ANFANG DEFINIEREN
  min <- reactive({
    as.Date(input$date[1], format = "%d.%m.%y")
  })
  
  # INTERVALL-ENDE DEFINIEREN
  max <- reactive({
    as.Date(input$date[2], format = "%d.%m.%y")
  })
  
  # DATEN NACH INSTITUT FILTERN
  filtered_data <- reactive({
    dataset %>% filter(Institut %in% input$institut)
  })
  
  
  output$plot1 <- renderPlotly({
    

  # FARBEN FESTLEGEN
   colors <- c("AfD" = "#019ee3", 
               "Andere" = "#7c7c7c", 
               "FDP" = "#fbeb04", 
               "Grüne" = "#1ca42c", 
               "Linke*" = "#bd3076", 
               "Piraten" = "#f39200",
               "SPD" = "#e2001a", 
               "Union" = "#000000")
   
                   
   plot1 <- filtered_data() %>% filter(between(Datum, as.Date(min()), as.Date(max()))) %>%
                 plot_ly(x = ~Datum, y = ~Prozent) %>%
                   add_lines(color = ~Partei, 
                             colors = colors,
                             linetype = ~as.factor(Institut)) %>%
                   layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min(), "%d.%m.%y")),
                          # legend = list(orientation = "h",
                          #               xanchor = "center",
                          #               x = .5,
                          #               y = -.45),
                          margin = c(1,1,1,1))
                   
    
  })
  
  

}




##### Applikation starten -----
shinyApp(ui = ui, server = server)


