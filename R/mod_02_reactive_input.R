#' 02_reactive_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_reactive_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    
 
  )
}
    
#' 02_reactive_input Server Function
#'
#' @noRd 
mod_02_reactive_input_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_02_reactive_input_ui("02_reactive_input_ui_1") - "completed"
    
## To be copied in the server
# callModule(mod_02_reactive_input_server, "02_reactive_input_ui_1") - "completed"
 
