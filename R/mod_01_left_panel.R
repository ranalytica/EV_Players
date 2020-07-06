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
        textInput(ns("symb"), "Stock Symbol", "SPY"),
   
        br(),
        br(),
        
        checkboxInput(ns("log"), "Plot y axis on log scale",
                      value = FALSE),
        
        checkboxInput(ns("adjust"),
                      "Adjust prices for inflation", value = FALSE)
      ),
      
      mainPanel(
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
  
 
  output$plot <- renderPlotly({
    
   p<- plot_ly(dataInput(), 
            x=~date, y=~close, mode = "line") %>%
                layout(title="Price History",
                  yaxis=list(title="Price"),
                  xaxis=list(title="Date",
                  rangeslider = list(type="date")))
   
   p1<- plot_ly(dataInput(),
            x=~close, mode="histogram") %>% 
              layout(title="Price Distribution",
                yaxis=list(title="Price"),
                xaxis=list(title="Date"))
   
   p3 <- plot_ly(dataInput(),
             x=~volume, y=~close, type="scatter")
  
   
   subplot(p, p1, p3, nrows = 2)
     
  })
 
  

  
}
    
## To be copied in the UI
# mod_01_left_panel_ui("01_left_panel_ui_1")
    
## To be copied in the server
# callModule(mod_01_left_panel_server, "01_left_panel_ui_1")
 
