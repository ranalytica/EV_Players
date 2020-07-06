#' 01_left_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

library(tidyquant)
library(tidyverse)
library(plotly)
mod_01_left_panel_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        helpText("Select a stock to examine.

        Information will be collected from Yahoo finance."),
        textInput(ns("symb"), "Symbol", "SPY"),
        
        dateRangeInput(ns("dates"),
                       "Date range",
                       start = "2013-01-01",
                       end = as.character(Sys.Date())),
        
        br(),
        br(),
        
        checkboxInput(ns("log"), "Plot y axis on log scale",
                      value = FALSE),
        
        checkboxInput(ns("adjust"),
                      "Adjust prices for inflation", value = FALSE)
      ),
      
      mainPanel(
        h4("Stock Price"),
        plotlyOutput(outputId = ns("plot"))
    )
    
  )
     
  )
  }


    
#' 01_left_panel Server Function
#'
#' @noRd 
mod_01_left_panel_server <- function(input, output, session){
  ns <- session$ns
  
  dataInput <- reactive({
    tq_get(input$symb,
               from = input$dates[1],
               to = input$dates[2])
  })
  
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  
  output$plot <- renderPlotly({
    
   p<- plot_ly(dataInput(), 
            x=~date, y=~close, mode = "line") 
              
   p %>% layout(autosize =F,
                         witdth =500,
                         height=500,
                         margin=5)
    
  })
 
  

  
}
    
## To be copied in the UI
# mod_01_left_panel_ui("01_left_panel_ui_1")
    
## To be copied in the server
# callModule(mod_01_left_panel_server, "01_left_panel_ui_1")
 
