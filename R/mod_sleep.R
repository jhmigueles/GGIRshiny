#' sleep UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param ggir_args GGIR arguments specified in mod_processing1 (datadir, outputdir, metadatadir,..).
#' @param ggir_args2 GGIR arguments specified in mod_distribution (strategy, maxdur,...).
#' @param parent_session Parent session.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets radioGroupButtons actionBttn checkboxGroupButtons
#' @importFrom graphics rect
#' @importFrom plotly plot_ly renderPlotly plotlyOutput
mod_sleep_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        div(
          uiOutput(ns("person2plot"))
        ),
        br(),
        div(
          actionButton(inputId = ns("sleeplog"), 
                       label = "Do you have a sleep log?",
                       width = "100%")
        ),
        div(
          uiOutput(ns("sleeplog_id"))
        ),
        div(
          uiOutput(ns("sleeplog_coln1"))
        ),
        div(
          uiOutput(ns("sleeplog_nnights"))
        ),
        br(),
        div(
          strong("Should the first and last days be excluded?"),
          prettyToggle(inputId = ns("excludefirstlast"),
                       label_on = "Yes",label_off = "No", 
                       outline = TRUE, plain = TRUE, bigger = TRUE,
                       icon_on = icon("exclamation-circle"),icon_off = icon("check-circle"),
                       status_on = "warning",status_off = "primary")
        ),
        br(),
        div(
          strong("Overwrite current files?"),
          prettyToggle(inputId = ns("overwrite"),
                       label_on = "Yes (remove current files)",label_off = "No", 
                       outline = TRUE, plain = TRUE, bigger = TRUE,
                       icon_on = icon("exclamation-circle"),icon_off = icon("check-circle"),
                       status_on = "warning",status_off = "primary")
        ),
        br(),
        fluidRow(
          col_6(
            actionButton(inputId = ns("run3"), icon = icon("play"),
                         label = "Run",
                         width = "100%")
          ),
          col_6(
            uiOutput(ns("goactivity"))
          )
        )
      ),
      mainPanel(
        fluidRow(
          plotlyOutput(ns("plot_sleep"))
        ),
        fluidRow(
          col_6(
            uiOutput(ns("ageGroup"))
          ),
          col_6(
            uiOutput(ns("recommendations"))
          )
        )
      )
    )
  )
}

#' sleep Server Function
#'
#' @noRd 
mod_sleep_server <- function(input, output, session, ggir_args, ggir_args2, parent_session){
  ns <- session$ns
  
  
  # PLOT EPOCH DATA ---------------------------------------------------------
  # Person to plot
  output$person2plot = renderUI({
    path = file.path(ggir_args$metadatadir(), "meta", "ms2.out")
    files = reactive(dir(path))
    div(
      pickerInput(inputId = ns("person"),
                  label = "Select the person to plot",
                  choices = files(),
                  selected = files()[1],
                  options = list(`live-search` = TRUE))
    )
  })
  
  # SLEEP LOG ---------------------------------------------------------------
  loglocation = NULL
  log_tmp = NULL
  makeReactiveBinding("loglocation")
  makeReactiveBinding("log_tmp")
  observeEvent(input$sleeplog,{
    # Reactive datadir
    loglocation <<- tk_choose.files(multi = FALSE)
    log_tmp <<- read.csv(loglocation)
    output$sleeplog_id = renderUI({
      div(
        radioGroupButtons(
          inputId = ns("sleeplog_id"),
          label = "Select the ID variable",
          choices = colnames(log_tmp),
          selected = colnames(log_tmp)[1],
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle", 
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o", 
                        style = "color: steelblue"))
        )
      )
      
    })
    output$sleeplog_coln1 = renderUI({
      div(
        radioGroupButtons(
          inputId = ns("sleeplog_coln1"),
          label = "Select the first sleep onset",
          choices = colnames(log_tmp),
          selected = colnames(log_tmp)[2],
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle", 
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o", 
                        style = "color: steelblue"))
        )
      )
    })
    output$sleeplog_nnights = renderUI({
      div(
        numericInput(
          inputId = ns("sleeplog_nnights"),
          label = "How many nights are in your sleep log?", value = 7, min = 1, step = 1
        )
      )
    })
  })
  
  # RUN G.PART3-4 -----------------------------------------------------------
  observeEvent(input$run3,{
    # Disable inputs while running
    input_list <- reactiveValuesToList(input)
    disable_inputs(input_list,F)
    
    # RUN GGIR ----------------------------------------------------------------
    # At the moment (2019-06-22) I use a for loop processing 1 file at a time. 
    # This way we can get the progress with every file being processed
    # Need to improve this in the future
    
    withProgress(message = 'Deriving sleep data', value = 0, {
      
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Processing files")
      
      colid = which(colnames(log_tmp) == input$sleeplog_colid)
      coln1 = which(colnames(log_tmp) == input$sleeplog_coln1)
      
      # Run g.part3 and g.part4
      GGIR::g.shell.GGIR(mode=3:4,         
                         #BASIC SETTINGS
                         datadir=ggir_args$datadir(),
                         outputdir=ggir_args$outputdir(),
                         studyname = ggir_args$studyname(),
                         f0 = 1, f1 = length(ggir_args$datadir()),
                         overwrite = input$overwrite, 
                         # PRE-PROCESSING AND METRICS (g.part1 and g.part2 arguments):
                         windowsizes = ggir_args$windowsizes(), 
                         desiredtz = ggir_args$desiredtz(),
                         # SLEEP PARAMETERS (g.part3 and g.part4 arguments):
                         loglocation = loglocation,
                         colid = colid, 
                         coln1 = coln1, 
                         nnights = input$sleeplog_nnights,
                         sleeplogidnum = FALSE,
                         do.visual=TRUE,
                         includenightcrit = 0,
                         excludefirstlast = input$excludefirstlast,
                         # REPORTS
                         do.report=c(4),
                         visualreport=FALSE, 
                         do.parallel = FALSE)
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Done")
      disable_inputs(input_list,T)
    })
    
    # Plot
    output$plot_angle = renderPlot({})
    output$plot_sleep = renderPlotly({
      data = read.csv(file.path(ggir_args$metadatadir(), "results", 
                                "part4_nightsummary_sleep_cleaned.csv"))
      nums = data[which(data$filename == input$person), "SleepDurationInSpt"]
      plot_ly(
        x = data[which(data$filename == input$person), "weekday"],
        y = nums,
        type = "bar"
      )
    })
    
    # Recommendations
    output$ageGroup = renderUI({
      div(
        radioGroupButtons(ns("ageGroup"), label = "Select the age group",
                          choices = c("Preschooler", "Child", "Adolescent", "Adult"),
                          selected = "Adult",
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue")))
      )
    })
    output$recommendations = renderUI({
      actionBttn(ns("recs"), label = "Reach sleep recommendations?",
                 style = "jelly", color = "danger", size = "sm", block = TRUE)
    })
  })
  
  
  # Meet recommendations? ---------------------------------------------------
  observeEvent(input$recs,{
    data = read.csv(file.path(ggir_args$metadatadir(), "results", "part4_summary_sleep_cleaned.csv"))
    sleepWD = (data[amatch(input$person, data$filename, maxDist = 6), "SleepDurationInSpt_WD_T5A5_mn"])
    sleepWE = (data[amatch(input$person, data$filename, maxDist = 6), "SleepDurationInSpt_WE_T5A5_mn"])
    meanSleep = (((sleepWD * 5) + (sleepWE * 2)) / 7)
    
    threshold = ifelse(input$ageGroup == "Preschooler", 11.5,
                       ifelse(input$ageGroup == "Child", 10, 
                              ifelse(input$ageGroup == "Adolescent", 9, 7)))
    
    if(meanSleep >= threshold){
      sendSweetAlert(
        session = session,
        title = "Yes!",
        text = "This participant sleeps enough time",
        type = "success",
        btn_labels = "Ok",
        btn_colors = "#3085d6"
      )
    } else if(meanSleep < threshold){
      sendSweetAlert(
        session = session,
        title = "No!",
        text = "This participant does not sleep enough time",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6"
      )
    }
  })
  
  
  # GO TO CHECK DATA QUALITY ------------------------------------------------
  output$goactivity = renderUI({
    actionBttn(inputId = ns("goactivity"), label = "Check physical activity data",
               style = "jelly", color = "danger", size = "sm", block = TRUE)
  })
  
  observeEvent(input$goactivity,{
    updateTabsetPanel(parent_session, "navbarpage", selected = "activity")
  })
  
  
  # RETURN ------------------------------------------------------------------
  return(list())
}
