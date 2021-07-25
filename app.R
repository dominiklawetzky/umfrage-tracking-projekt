source("data.R", local = TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(plotly)

#TEST


##### UI ------

ui <- fluidPage(theme = shinythemes::shinytheme("simplex"),
                titlePanel("Umfrage-Tracking-Projekt"),
      
                  sidebarPanel(
                    conditionalPanel(condition="input.tabselected == 1",
                    
                    # ZEITINTERVALL AUSWaeHLEN
                    dateRangeInput("date", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-07-20",
                                   min = "2010-01-01", max = "2021-07-20",
                                   format = "dd.mm.yyyy", language = "de", separator = "bis"),
                    
                    # INSTITUT AUSWaeHLEN
                    checkboxGroupInput(inputId = "institut", label = strong("Umfrage-Institute"),
                                       selected = unique(dataset$Institut),
                                       choices = unique(dataset$Institut)),
                    
                    # BUNDESTAGSWAHL ANZEIGEN
                    checkboxInput(inputId = "btw", label = strong("Bundestagswahlen"),
                                       value = FALSE),
                  
                    
                    # PARTEIEN AUSWaeHLEN
                    checkboxGroupInput(inputId = "partei", label = strong("Parteien"),
                                       selected = unique(dataset$Partei),
                                       choices = unique(dataset$Partei)),
                    
                    # ERKLaeRUNG ZU LINKEN
                    HTML("<em>(*) Bei Infratest bis zum 10.06.2005 nur PDS, ab Juli 2007 \"DIE LINKE\"</em><br><br>"),
                    HTML("<strong>Zuletzt aktualisiert:</strong><p>20.07.2021</p>")
                  
                    
                    ),
                    
                    conditionalPanel(condition="input.tabselected == 2",
                                     
                                     # ZEITINTERVALL AUSWaeHLEN
                                     dateRangeInput("date", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-07-20",
                                                    min = "2010-01-01", max = "2021-07-20",
                                                    format = "dd.mm.yyyy", language = "de", separator = "bis"),
                                     
                                     # INSTITUT AUSWaeHLEN
                                     selectInput(inputId = "institut_sel", label = strong("Umfrage-Institute"),
                                                        selected = c("Infratest"),
                                                        choices = unique(dataset$Institut)),
                                     
                                     # PARTEI AUSWaeHLEN
                                     selectInput(inputId = "partei_sel", label = strong("Parteien"),
                                                        selected = c("Union"),
                                                        choices = unique(dataset$Partei)),
                                     
                                     
                                     # LOESS-GLaeTTUNG JA/NEIN
                                     checkboxInput(inputId = "smoother", label = strong("LOESS-Glaettung aktivieren"), value = FALSE),
                                     
                                     # SPAN DER LOESS-GLaeTTUNG
                                     conditionalPanel(condition = "input.smoother == true",
                                                      sliderInput(inputId = "span", label = "Spannweite:",
                                                                  min = 1, max = 10, value = 5, step = 0.01,
                                                                  animate = animationOptions(interval = 100)),
                                                      HTML("Je gr??er der Wert, desto staerker die Glaettung."))
                                     
                    )
                    

                  ),
                  
        
                  # OUTPUT-PANEL
                  
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                
                                # TAB 1
                                tabPanel("?bersicht", value = 1,
                                         
                                         plotlyOutput("plot1",
                                                      height = "auto", width = "auto"),
                                         includeHTML("infos.html")
                                         ),
                                
                                # TAB 2
                                tabPanel("Trendanalyse", value = 2,
                                         plotlyOutput("plot2",
                                                      height = "auto", width = "auto"),
                                         includeHTML("trendanalyse.html")),
                                
                                # TAB 3
                                tabPanel("...", 
                                         plotOutput("deaths",
                                                    width = "100%")),
                                
                                # TAB 4
                                tabPanel("Info",
                                         h2("Plots herunterladen"),
                                         p("Die Plots k?nnen ?ber einen Rechtsklick mittels \"Speichern unter\" und ggf. unter Hinzuf?gen des Suffix \".png\" gespeichert werden."),
                                         h2("Glaettungsmethoden"),
                                         HTML("<p>lm: linear regression model <br> glm: generalized linear model <br> gam: generalized additive mode <br> loess: locally estimated scatterplot smoothing</p>")),
                                
                                id = "tabselected"
                                
                               ),
                    
                    # FOOTER MIT IMPRESSUM
                    tags$footer(HTML("<a href=\"https://dominiklawetzky.de/impressum\">Impressum</a>"), align = "center", style ="margin-bottom:1em; margin-top: 2em")
                    
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
  
  # DATEN NACH INSTITUT / PARTEI FILTERN
  filtered_data <- reactive({
    if(input$tabselected == 1){
      dataset %>% filter(Institut %in% input$institut) %>%
        filter(Partei %in% input$partei)
    }
    else if(input$tabselected == 2){
      if(input$smoother == FALSE) {
        dataset %>% filter(Institut %in% input$institut_sel) %>%
          filter(Partei %in% input$partei_sel)
      }
      else {
        dat <- dataset %>% filter(Institut %in% input$institut_sel) %>%
          filter(Partei %in% input$partei_sel)
        
        dat$ID <- 1:nrow(dat) # ID hinzuf?gen f?r LOESS
        
        dat$smoothed <- ksmooth(dat$ID, dat$Prozent, kernel = "normal", bandwidth = input$span)$y # LOESS-Glaettung anwenden
        
        print(dat) # Ausgabe
      }
    }
  })
  
  
  output$plot1 <- renderPlotly({
    

  # FARBEN FESTLEGEN
   colors <- c("AfD" = "#019ee3", 
               "Andere" = "#7c7c7c", 
               "FDP" = "#fbeb04", 
               "Gr?ne" = "#1ca42c", 
               "Linke*" = "#bd3076", 
               "Piraten" = "#f39200",
               "SPD" = "#e2001a", 
               "Union" = "#000000")
   
   
  # GERADE DEFINIEREN
   vline <- function(x, color = "red") {
     list(
       type = "line", 
       y0 = 0, 
       y1 = 1, 
       yref = "paper",
       x0 = x, 
       x1 = x, 
       line = list(color = color)
     )
   }
   
  
   # PLOT 1
   plot1 <- filtered_data() %>% filter(between(Datum, as.Date(min()), as.Date(max()))) %>%
     plot_ly(x = ~Datum, y = ~Prozent) %>%
     add_lines(color = ~Partei, 
               colors = colors,
               linetype = ~as.factor(Institut)) %>%
     layout(title = sprintf("Zustimmungswerte der gro?en politischen Parteien seit %s", format(min(), "%d.%m.%y")),
            margin = c(1,1,1,1))
   
  if(input$btw == TRUE) {
    plot1 <- plot1 %>% layout(shapes = list(vline("2013-09-22"), vline("2017-09-24"))) # Plot 1 MIT Bundestagswahlen
  }
   else {
     plot1 # Plot 1 OHNE Bundestagswahlen
   }

    
  })
  
  output$plot2 <- renderPlotly({
    
    
    # FARBEN FESTLEGEN
    colors <- c("AfD" = "#019ee3", 
                "Andere" = "#7c7c7c", 
                "FDP" = "#fbeb04", 
                "Gr?ne" = "#1ca42c", 
                "Linke*" = "#bd3076", 
                "Piraten" = "#f39200",
                "SPD" = "#e2001a", 
                "Union" = "#000000")
    
    # PLOT OHNE LOESS
    if(input$smoother == FALSE){
    plot2 <- filtered_data() %>% filter(between(Datum, as.Date(min()), as.Date(max()))) %>%
        plot_ly(x = ~Datum, y = ~Prozent) %>%
        add_lines(color = ~Partei, 
                  colors = colors) %>%
        layout(title = sprintf("Zustimmungswerte der gro?en politischen Parteien seit %s", format(min(), "%d.%m.%y")),
               margin = c(1,1,1,1),
               annotations = 
                 list(x = 1, y = -0.2, text = sprintf("Quelle: %s", input$institut_sel), 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=12, color="grey")))
    }
    
    # PLOT MIT LOESS
    else {
      plot2_loess <- filtered_data() %>% filter(between(Datum, as.Date(min()), as.Date(max()))) %>%
          plot_ly(x = ~Datum, y = ~smoothed) %>%
          add_lines(color = ~Partei, 
                    colors = colors) %>%
          layout(title = sprintf("Zustimmungswerte der gro?en politischen Parteien seit %s", format(min(), "%d.%m.%y")),
                 yaxis = list(title = "Prozent (geglaettet)"),
                 margin = c(1,1,1,1),
                 annotations = 
                   list(x = 1, y = -0.2, text = sprinstf("Quelle: %s", input$institut_sel), 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=12, color="grey"))) %>%
          layout(annotations = 
                 list(x = .6 , y = 1.175, text = "Local Polynomial Regression Fitting", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=14, color="grey")))
    }
    
    
  })
  
  

}



##### Applikation starten -----
shinyApp(ui = ui, server = server)


