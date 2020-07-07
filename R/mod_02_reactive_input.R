#' 02_reactive_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
library(DT)
library(RedditExtractoR)
library(dplyr)
library(lubridate)
library(gt)

mod_02_reactive_input_ui <- function(id){
  ns <- NS(id)
  tagList(
   
    gt_output(ns("tbl"))
    
 
  )
}
    
#' 02_reactive_input Server Function
#'
#' @noRd 
mod_02_reactive_input_server <- function(input, output, session, RedditLinks){
  ns <- session$ns
  
RedditLinks <- reactive({ 
  
    reddit_urls(
      search_terms   = glue::glue(input$symb, " Stock"),
      page_threshold = 1,
      sort_by = "new",
      wait_time = 3,
      subreddit = "wallstreetbets"
    )
  })
  
RedditLinks1 <- reactive({ 
  
  RedditLinks() %>% 
    mutate(link= paste("[",title,"](",URL,")")
    )
  })

RedditDate <- reactive({ 
  RedditLinks1() %>% mutate(date=dmy(date) 
                            )
})

RedditDateA <- reactive({ 
RedditDate() %>% dplyr::arrange(desc(date))
  
})
link <- reactive({ 
  RedditDateA() %>% select(date,title)
})

gt_link <- reactive({   
   
link() %>% gt() %>% tab_header(
  title = glue::glue(input$symb," Stock Reddit post from WallStreetBets forum"))
  
  }) 

output$tbl = gt::render_gt(
  gt_link()
)
}
    
## To be copied in the UI
# mod_02_reactive_input_ui("02_reactive_input_ui_1") - "completed"
    
## To be copied in the server
# callModule(mod_02_reactive_input_server, "02_reactive_input_ui_1") - "completed"
 
