#setwd("/Users/dominiklawetzky/Documents/GitHub/sonntagsfrage/App")
source("data.R", local = TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(plotly)
library(shinythemes)


# Version 0.4.2

##### UI ------

ui <- fluidPage(theme = shinythemes::shinytheme("simplex"),
                
                titlePanel("Umfrage-Tracking-Projekt"),
                
                sidebarPanel(
                  conditionalPanel(condition="input.tabselected == 1",
                                   
                                   # ZEITINTERVALL AUSWaeHLEN
                                   dateRangeInput("date1", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-08-23",
                                                  min = "2010-01-01", max = "2021-08-23",
                                                  format = "dd.mm.yyyy", language = "de", separator = "bis"),
                                   
                                   # INSTITUT AUSWaeHLEN
                                   checkboxGroupInput(inputId = "institut", label = strong("Umfrage-Institute"),
                                                      selected = unique(dataset$Institut),
                                                      choices = unique(dataset$Institut)),
                                   
                                   # BUNDESTAGSWAHL ANZEIGEN
                                   checkboxInput(inputId = "btw", label = strong("Bundestagswahlen"),
                                                 value = FALSE),
                                   
                                   
                                   # PARTEIEN AUSWÄHLEN
                                   checkboxGroupInput(inputId = "partei", label = strong("Parteien"),
                                                      selected = unique(dataset$Partei),
                                                      choices = unique(dataset$Partei)),
                                   
                                   # ERKLÄRUNG ZU LINKEN
                                   HTML("<em>(*) Bei Infratest bis zum 10.06.2005 nur PDS, ab Juli 2007 \"DIE LINKE\"</em><br><br>"),
                                   HTML("<strong>Zuletzt aktualisiert:</strong><p>23.08.2021</p>"),
                                   HTML("<a class=\"github-button\" href=\"https://github.com/dominiklawetzky/umfrage-tracking-projekt\" data-icon=\"octicon-star\" 
                        data-size=\"large\" data-show-count=\"true\" 
                        aria-label=\"Star dominiklawetzky/umfrage-tracking-projekt on GitHub\">GitHub Repository</a>")
                                   
                                   
                  ),
                  
                  conditionalPanel(condition="input.tabselected == 2",
                                   
                                   # ZEITINTERVALL AUSWÄHLEN
                                   dateRangeInput("date2", strong("Betrachtungszeitraum"), start = "2010-01-01", end = "2021-07-20",
                                                  min = "2010-01-01", max = "2021-07-20",
                                                  format = "dd.mm.yyyy", language = "de", separator = "bis"),
                                   
                                   # INSTITUT AUSWÄHLEN
                                   selectInput(inputId = "institut_sel", label = strong("Umfrage-Institute"),
                                               selected = c("Infratest"),
                                               choices = unique(dataset$Institut)),
                                   
                                   # PARTEI AUSWÄHLEN
                                   selectInput(inputId = "partei_sel", label = strong("Parteien"),
                                               selected = c("Union"),
                                               choices = unique(dataset$Partei)),
                                   
                                   
                                   # LOESS-GLÄTTUNG JA/NEIN
                                   checkboxInput(inputId = "smoother", label = strong("LOESS-Glättung aktivieren"), value = FALSE),
                                   
                                   # SPAN DER LOESS-GLÄTTUNG
                                   conditionalPanel(condition = "input.smoother == true",
                                                    sliderInput(inputId = "span", label = "Spannweite:",
                                                                min = 1, max = 10, value = 5, step = 0.01,
                                                                animate = animationOptions(interval = 100)),
                                                    HTML("Je gröer der Wert, desto staerker die Glättung."))
                                   
                  )
                  
                  
                ),
                
                
                # OUTPUT-PANEL
                
                mainPanel(
                  
                  tabsetPanel(type = "tabs",
                              
                              # TAB 1
                              tabPanel("Übersicht", value = 1,
                                       plotlyOutput("plot1",
                                                    height = "650px", width = "100%"),
                                       includeHTML("HTML/infos.html")),
                              
                              # TAB 2
                              tabPanel("Trendanalyse", value = 2,
                                       plotlyOutput("plot2",
                                                    height = "auto", width = "auto"),
                                       includeHTML("HTML/infos.html")),
                              
                              # TAB 3
                              # tabPanel("...", 
                              #          plotOutput("deaths",
                              #                     width = "100%")),
                              
                              # TAB 4
                              tabPanel("Info", value = 4, includeHTML("HTML/info-tab.html")),
                              
                              id = "tabselected"
                              
                  ),
                  
                  # FOOTER MIT IMPRESSUM
                  tags$footer(HTML("<a href=\"https://dominiklawetzky.de/impressum\">Impressum</a>"), align = "center", style ="margin-bottom:1em; margin-top: 2em")
                  
                )
)

                                      

##### Server Logic ------
server <- function(input, output, session) {
  
  colors <-  reactive({
                c("AfD" = "#019ee3", 
                  "Andere" = "#7c7c7c", 
                  "FDP" = "#fbeb04", 
                  "Freie Wähler" = "#084e8b",
                  "Grüne" = "#1ca42c", 
                  "Linke*" = "#bd3076", 
                  "Piraten" = "#f39200",
                  "SPD" = "#e2001a", 
                  "Union" = "#000000")
    })

  
  # INTERVALL-ANFANG DEFINIEREN
  min <- reactive({
    if(input$tabselected == 1) {
      as.Date(input$date1[1], format = "%d.%m.%y")
    }
    else if (input$tabselected == 2) {
      as.Date(input$date2[1], format = "%d.%m.%y")
    }
  })
  
  
  # INTERVALL-ENDE DEFINIEREN
  max <- reactive({
    if(input$tabselected == 1) {
      as.Date(input$date1[2], format = "%d.%m.%y")
    }
    else if (input$tabselected == 2) {
      as.Date(input$date2[2], format = "%d.%m.%y")
    }
  })
  
  
  # DATEN NACH INSTITUT / PARTEI FILTERN / DATUM
  filtered_data <- reactive({
    if(input$tabselected == 1){
      dataset %>% filter(Institut %in% input$institut) %>% # Filter nach Institut
        filter(Partei %in% input$partei) # Filter nach Partei
    }
    else if(input$tabselected == 2){
      if(input$smoother == FALSE) {
        dataset %>% filter(Institut %in% input$institut_sel) %>% # Filter nach Institut
          filter(Partei %in% input$partei_sel) %>% # Filter nach Partei
            filter(between(Datum, as.Date(min()), as.Date(max()))) %>% # Filter nach Datum
            print()
      }
      else {
        dat <- dataset %>% filter(Institut %in% input$institut_sel) %>%
          filter(Partei %in% input$partei_sel)
        
        dat$ID <- 1:nrow(dat) # ID hinzufügen für LOESS
        
        dat$smoothed <- ksmooth(dat$ID, dat$Prozent, kernel = "normal", bandwidth = input$span)$y # LOESS-Glättung anwenden
        
        dat %>% 
          filter(between(Datum, as.Date(min()), as.Date(max()))) %>% 
          print() # Ausgabe
      }
    }
  })
  
  # PLOT-GRÖSSE SPEICHERN
  plot_size <- reactive({
    session$clientData$output_plot1_width
  })
  
  
  output$plot1 <- renderPlotly({
   
   
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
               colors = colors(),
               linetype = ~as.factor(Institut)) %>%
     layout(legend = list(font = list(size = 12)),
            dragmode = "select",
            autosize = TRUE
            ) %>%
     config(locale = "de")
   
   
  # BTW ANZEIGEN / NICHT ANZEIGEN
  if(input$btw == TRUE) {
    
    # RESPONSIVE LEGENDE
    if(plot_size() >= 800) {
      plot1 %>% layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min(), "%d.%m.%y")),
                       shapes = list(vline("2013-09-22"), vline("2017-09-24")),
                       margin = c(1,1,1,1)) # Plot 1 MIT Bundestagswahlen
    }
    else {
      plot1 %>% layout(shapes = list(vline("2013-09-22"), vline("2017-09-24"))) %>%
        layout(title = sprintf("Zustimmungswerte der großen \npolitischen Parteien seit %s", format(min(), "%d.%m.%y")),
               legend = list(orientation = "h",
                             xanchor = "center",
                             yanchor = "top",
                             x = .5,
                             y = -.8,
                             font = list(size = 12)),
               margin = c(0,0,0,0))
    }
    
  }
   else {
     
     # RESPONSIVE LEGENDE
     if(plot_size() >= 800) {
       plot1 %>% layout(title = sprintf("Zustimmungswerte der großen politischen Parteien seit %s", format(min(), "%d.%m.%y")), 
                        margin = c(1,1,1,1))
     }
     else {
       plot1 %>% layout(title = sprintf("Zustimmungswerte der großen \npolitischen Parteien seit %s", format(min(), "%d.%m.%y")),
                        legend = list(orientation = "h",
                                      xanchor = "center",
                                      yanchor = "top",
                                      x = .5,
                                      y = -.8,
                                      font = list(size = 12)),
                        margin = c(0,0,0,0))
     }
     
   }
    

  })
  
  
  
  output$plot2 <- renderPlotly({
    
    # PLOT OHNE LOESS
    if(input$smoother == FALSE){
    plot2 <- filtered_data() %>%
        plot_ly(x = ~Datum, y = ~Prozent) %>%
        add_lines(color = ~Partei, 
                  colors = colors()) %>%
        layout(title = sprintf("Zustimmungswerte der %s", input$partei_sel),
               margin = c(1,1,1,1),
               annotations = 
                 list(x = 1, y = -0.2, text = sprintf("Quelle: %s", input$institut_sel), 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=12, color="grey"))) %>%
      config(locale = "de")
    }
    
    
    # PLOT MIT LOESS
    else {
      plot2_loess <- filtered_data() %>%
          plot_ly(x = ~Datum, y = ~smoothed) %>%
          add_lines(color = ~Partei, 
                    colors = colors()) %>%
          layout(title = sprintf("Zustimmungswerte der %s \nLocal Polynomial Regression Fitting", input$partei_sel),
                 yaxis = list(title = "Prozent (geglättet)"),
                 margin = c(1,1,1,1),
                 annotations = 
                   list(x = 1, y = -0.2, text = sprintf("Quelle: %s", input$institut_sel), 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=12, color="grey"))) %>%
        config(locale = "de")
    }
  })
  
  

}


##### Applikation starten -----
shinyApp(ui = ui, server = server)


