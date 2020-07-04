#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # About tab ---------------------------------------------------------------
  callModule(mod_about_server, "about_ui_1", parent_session = session)
  
  
  # Raw data processing -----------------------------------------------------
  # DATA AGGREGATION
  ggir_args = callModule(mod_processing1_server, "processing1_ui_1", parent_session = session)
  
  # DATA SELECTION & DISTRIBUTION
  ggir_args2 = callModule(mod_distribution_server, "distribution_ui_1", 
                          ggir_args = ggir_args, parent_session = session)
  
  # SLEEP
  callModule(mod_sleep_server, "sleep_ui_1",
             ggir_args = ggir_args, ggir_args2 = ggir_args2,
             parent_session = session)
  
  # PHYSICAL ACTIVITY
  callModule(mod_activity_server, "activity_ui_1",
             ggir_args = ggir_args, ggir_args2 = ggir_args2)
}
