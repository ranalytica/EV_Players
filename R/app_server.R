#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_01_left_panel_server, "01_left_panel_ui_1")
  #callModule(mod_02_reactive_input_server, "02_reactive_input_ui_1")
}
