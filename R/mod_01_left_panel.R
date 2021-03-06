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
    sidebarPanel(
      textInput(ns("symb"), "Stock Symbol", "NIO"),
    
    dateRangeInput(ns("dates"),
                   "Date range",
                   start = "2013-01-01",
                   end = as.character(Sys.Date())),width=2),
    
    mainPanel(plotly::plotlyOutput(outputId = ns("plot")),
              gt_output(ns("tbl")))
  ))
}



#' 01_left_panel Server Function
#'
#' @noRd
mod_01_left_panel_server <-
  function(input, output, session, symb) {
    ns <- session$ns
    
    dataInput <- reactive({
      tq_get(input$symb,
             from = input$dates[1],
             to = input$dates[2])
    })
    
    dataInput1 <- reactive({
      dataInput() %>% dplyr::mutate(RedGreenDay =
                                      ifelse(close >= open, "+ day", "- day"))
    })
    
    
    
    output$plot <- renderPlotly({
      
      p1Ann <- list(
        text = glue::glue(input$symb, " Price History"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      )
      
      p2Ann <- list(
        text = glue::glue(input$symb, " Price Distribution"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = .8,
        showarrow = FALSE)
      
      p3Ann <-list(
        text = glue::glue(input$symb, " Price/Volume"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE)
      
      p4Ann <-list(
        text = glue::glue(input$symb, " Volume/Date"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = .8,
        showarrow = FALSE)
      
      p1 <- plotly::plot_ly(dataInput(),
                   x =  ~ date,
                   y =  ~ close,
                   mode = "line") %>%
        layout(annotations =p1Ann,
          yaxis = list(title = "Price"),
          xaxis = list(title = "")
        )
      
      p2 <- plotly::plot_ly(dataInput(),
                    x =  ~ close, mode = "histogram") %>%
        layout(annotations = p2Ann,
          yaxis = list(title = "Frequency"),
          xaxis = list(title = "")
        )
      
      p3 <- plotly::plot_ly(
        dataInput1(),
        x =  ~ volume,
        y =  ~ close,
        type = "scatter",
        color = ~ RedGreenDay,
        colors = c("red", "dark green"),
        opacity = .5
      ) %>% layout(annotations = p3Ann, 
                   yaxis=list(title= "Price")
                   ,xaxis = list(title = "Volume"))
      
      p4 <- plotly::plot_ly(
        dataInput1(),
        x =  ~ date,
        y =  ~ volume,
        type = "scatter",
        color = ~ RedGreenDay,
        colors = c("red", "dark green"),
        opacity = .5
      ) %>% layout(annotations = p4Ann, 
                   yaxis=list(title= "Volume"),
                   xaxis = list(title = "Date"))
      
      plotly::subplot(list(p1, p2, p4, p3), 
                      nrows = 2, 
                      margin = .07,
                      titleX = T,
                      titleY = T
                      ) %>% 
                      plotly::layout(
                      showlegend = F
                      )
      })
   
}

## To be copied in the UI
# mod_01_left_panel_ui("01_left_panel_ui_1")

## To be copied in the server
# callModule(mod_01_left_panel_server, "01_left_panel_ui_1")
