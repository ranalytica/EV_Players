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
library(dplyr)
library(ggplot2)
library(plotly)
library(glue)
mod_01_left_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(sidebarLayout(
    sidebarPanel(textInput(ns("symb"), "Stock Symbol", "NIO"), width = 2),
    
    mainPanel(plotlyOutput(outputId = ns("plot")))
  ))
}



#' 01_left_panel Server Function
#'
#' @noRd
mod_01_left_panel_server <-
  function(input, output, session, dataInput) {
    ns <- session$ns
    
    dataInput <- reactive({
      tq_get(input$symb)
    })
    
    dataInput1 <- reactive({
      dataInput() %>% dplyr::mutate(RedGreenDay =
                                      ifelse(close >= open, "+ day", "- day"))
    })
    
    output$plot <- renderPlotly({
      p <- plot_ly(dataInput(),
                   x =  ~ date,
                   y =  ~ close,
                   mode = "line") %>%
        layout(
          title = "Price History",
          yaxis = list(title = "Price"),
          xaxis = list(title = "Date")
        )
      
      p1 <- plot_ly(dataInput(),
                    x =  ~ close, mode = "histogram") %>%
        layout(
          title = "Price Distribution",
          yaxis = list(title = "Price"),
          xaxis = list(title = "Date")
        )
      
      p3 <- plot_ly(
        dataInput1(),
        x =  ~ volume,
        y =  ~ close,
        type = "scatter",
        color = ~ RedGreenDay,
        colors = c("red", "dark green"),
        opacity = .5
      )
      
      fig <- subplot(p1, p, p3, nrows = 1, margin = .05)
      
      fig %>% layout(
        title = glue(input$symb, " Price~Distribution, History and Volume"),
        showlegend = F
      )
    })
}

## To be copied in the UI
# mod_01_left_panel_ui("01_left_panel_ui_1")

## To be copied in the server
# callModule(mod_01_left_panel_server, "01_left_panel_ui_1")
