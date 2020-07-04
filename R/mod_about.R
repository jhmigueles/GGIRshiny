#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets actionBttn sendSweetAlert 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h1("Welcome to GGIRshiny"),
      h4("A fresh click-based interface to use the GGIR package open-source algorithms for
           generating and visualizing physical acivity and sleep descriptors from multi-day
           raw accelerometer data")
    ),
    fluidRow(
      hr(),
      h3("About GGIR"),
      p(strong("GGIR"), " facilitates the processing and extraction of insightful physical 
        activity and sleep variables of the data collected with these so called raw data 
        accelerometers. Overall description of the  GGIR software structure and functionalities 
        can be found",
        enurl("https://journals.humankinetics.com/view/journals/jmpb/2/3/article-p188.xml",
              span(strong("here"), class = "red")
        )
      )
    ),
    fluidRow(
      hr(),
      h3("What can you do with GGIRshiny?"),
      list_to_li(
        list("Process your raw data files to obtain day-by-day and person-by-person datasets.",
             "Visualize your already-processed datasets",
             "Investigate the participants data with interactive plots"
        ),
        class = "unorderedList"
      ),
      br(),
      p(strong("Start using GGIRshiny by selecting whether you want to process or visualize 
               data at the top")
      )
    ),
    fluidRow(
      hr(),
      h3("Citing GGIR"),
      p("Do not forget to cite GGIR in your publications via a version number and", 
        enurl("https://journals.humankinetics.com/view/journals/jmpb/2/3/article-p188.xml",
              span(strong(with_red_star("Migueles et al. 2019.")), class = "red")))
    ),
    fluidRow(
      hr(),
      col_6(
        h3("Contribute"),
        list_to_li(
          list(
            enurl("https://github.com/wadpac/GGIR/blob/master/README.md,%22here%22",
                  span(strong("GGIR"), class = "red")),
            enurl("https://github.com/jhmigueles/GGIRshiny",
                  span(strong("GGIRshiny"), class = "red"))
          ),
          class = "unorderedList"
        )
      ),
      col_6(
        rep_br(2),
        actionBttn(inputId = ns("start"), label = "Let's get started!", icon = icon("play"),
                   style = "jelly", color = "danger", size = "lg", block = TRUE)
      )
    ),
    fluidRow(
      hr(),
      p(with_red_star(""),"See the" ,
        enurl("https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html",
              span(strong("GGIR vignette"), class = "red")), "for more information"),
      br(),
      wellPanel("IMPORTANT: This app is under development. 
                     Its functionality is limited at the moment.",
                class = "redcenter"),
      p("Contacts: v.vanhees@movementdata.nl, jairohm@ugr.es", class = "right")
      
      
      
    )
    
  )
  
}

#' about Server Function
#'
#' @noRd 
mod_about_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  observeEvent(input$start,{
    updateTabsetPanel(parent_session, "navbarpage", selected = "aggregation")
  })
  
}