#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      navbarPage( id = "navbarpage",
                  title = enurl("https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html",
                                span(strong("GGIRshiny"), class="red")),
                  
                  
                  # About tab ---------------------------------------------------------------
                  tabPanel(
                    span(strong("About"), class="red"),
                    mod_about_ui("about_ui_1")
                  ),
                  
                  # Data processing app -----------------------------------------------------
                  # navbarMenu(
                  #   title = "Raw data processing", icon = icon("cog"),
                  tabPanel(
                    title = "Data aggregation", value = "aggregation",icon = icon("cog"),
                    mod_processing1_ui("processing1_ui_1")
                  ),
                  tabPanel(
                    title = "Acceleration distribution", value = "distribution", icon = icon("chart-bar"),
                    mod_distribution_ui("distribution_ui_1")
                  ),
                  tabPanel(
                    "Sleep ", value = "sleep", icon = icon("bed"),
                    mod_sleep_ui("sleep_ui_1")       
                  ),
                  tabPanel(
                    "Physical activity", value = "activity", icon = icon("walking"),
                    mod_activity_ui("activity_ui_1")
                  ) 
      )  
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  add_resource_path(
    'www', app_sys('app/www')
  )
  addResourcePath( 
    'img', app_sys('app/img')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GGIRshiny'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    tags$script(src="www/handlers.js"),
    tags$link(rel="stylesheet", type = "text/css", href = "www/custom.css")
  )
}

