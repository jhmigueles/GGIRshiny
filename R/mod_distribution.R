#' distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param ggir_args GGIR arguments specified in mod_processing1 (datadir, outputdir, metadatadir,..).
#' @param parent_session Parent session.
#'
#' @noRd 
#'
#' @import GGIR 
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets awesomeRadio 
#' @importFrom htmltools img 
#' @importFrom stringdist amatch
#' @importFrom plyr round_any
mod_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        div(
          awesomeRadio(inputId = ns("protocol"), label = "What option describes your protocol the best?",
                       choices = c("All data", "First and last midnight",
                                   "First midnight", "Specific hours", 
                                   "Most active days"), selected = "All data",status = "primary"
          )
        ),
        fluidRow(
          col_6(
            div(
              uiOutput(ns("hrs.del.start"))
            )
          ),
          col_6(
            div(
              uiOutput(ns("hrs.del.end"))
            )
          )
        ),
        fluidRow(
          col_6(
            uiOutput(ns("ndayswindow")))
        ),
        br(),
        div(
          sliderInput(inputId = ns("includedaycrit"), label = "Number of hours to consider a valid day",
                      min = 0, max = 24, step = 1, value = 16)
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
            actionButton(inputId = ns("run2"), icon = icon("play"),
                         label = "Run",
                         width = "100%")
          ),
          col_6(
            uiOutput(ns("gosleep"))
          )
        )
      ),
      mainPanel(
        fluidRow(
          div(
            uiOutput(ns("img"))
          )
        ),
        fluidRow(
          col_6(
            div(
              tableOutput(ns("person2plot"))
            )
          ),
          col_6(
            div(
              tableOutput(ns("acc2plot"))
            )
          )
        ),
        fluidRow(
          br(),
          col_6(
            div(
              plotlyOutput(ns("plot_mx"))
            )
          ),
          col_6(
            div(
              plotlyOutput(ns("plot_ilevels"))
            )
          )
        )
      )
    )
  )
}

#' distribution Server Function
#'
#' @noRd 
mod_distribution_server <- function(input, output, session, ggir_args, parent_session){
  ns <- session$ns
  
  # Protocol ----------------------------------------------------------------
  strategy = NULL; hrs.del.start = NULL; hrs.del.end = NULL; ndayswindow = NULL
  makeReactiveBinding("strategy")
  makeReactiveBinding("hrs.del.start")
  makeReactiveBinding("hrs.del.end")
  makeReactiveBinding("ndayswindow")
  
  observeEvent(input$protocol,{
    # Process all data available
    if(input$protocol == "All data"){
      output$img = renderUI({tags$img(src="img/protocol1.PNG", width = "100%")}) 
      output$hrs.del.start = renderUI({});output$hrs.del.end = renderUI({});output$ndayswindow = renderUI({})
      # Arguments
      strategy <<- 1
      hrs.del.start <<- 0
      hrs.del.end <<- 0
      ndayswindow <<- NULL
      # From first to last midnight
    } else if(input$protocol == "First and last midnight") {
      output$img = renderUI({
        tags$img(src="img/protocol2.PNG", width = "100%")
      }) 
      # Render UI
      output$hrs.del.start = renderUI({})
      output$hrs.del.end = renderUI({})
      output$ndayswindow = renderUI({})
      # Arguments
      strategy <<- 2
      hrs.del.start <<- NULL
      hrs.del.end <<- NULL
      ndayswindow <<- NULL
      # From first midnight
    } else if(input$protocol == "First midnight") {
      output$img = renderUI({
        tags$img(src="img/protocol3.PNG", width = "100%")
      }) 
      # Render UI
      output$hrs.del.start = renderUI({})
      output$hrs.del.end = renderUI({})
      output$ndayswindow = renderUI({})
      # Arguments
      strategy <<- 4
      hrs.del.start <<- NULL
      hrs.del.end <<- NULL
      ndayswindow <<- NULL
      # hrs.del.start / end
    } else if(input$protocol == "Specific hours") {
      output$img = renderUI({
        tags$img(src="img/protocol4.PNG", width = "100%")
      }) 
      # Render UI
      output$hrs.del.start = renderUI({
        numericInput(inputId = ns("hrs.del.start"), 
                     label = "Hours to exclude at start", 
                     value = 0.5,min = 0, step = 0.1)
      })
      output$hrs.del.end = renderUI({
        numericInput(inputId = ns("hrs.del.end"), 
                     label = "Hours to exclude at end", 
                     value = 0.5,min = 0, step = 0.1)
      })
      output$ndayswindow = renderUI({})
      # Arguments
      strategy <<- 1
      hrs.del.start <<- input$hrs.del.start
      hrs.del.end <<- input$hrs.del.end
      ndayswindow <<- NULL
      # MOST ACTIVE DAYS
    } else if(input$protocol == "Most active days") {
      output$img = renderUI({
        tags$img(src="img/protocol5.PNG", width = "100%")
      }) 
      # Render UI
      output$hrs.del.start = renderUI({})
      output$hrs.del.end = renderUI({})
      output$ndayswindow = renderUI({
        numericInput(inputId = ns("ndayswindow"), 
                     label = "Days to include", 
                     value = 7,min = 1, step = 1)
      })
      # Arguments
      strategy <<- 3
      hrs.del.start <<- NULL
      hrs.del.end <<- NULL
      ndayswindow <<- input$ndayswindow
    }
  })
  
  
  # RUN GGIR ----------------------------------------------------------------
  folder = reactive(file.path(ggir_args$metadatadir(), "results"))
  
  data_wk = NULL
  data_day = NULL
  
  makeReactiveBinding("data_wk")
  makeReactiveBinding("data_day")
  
  observeEvent(input$run2,{
    # Disable inputs while running
    input_list <- reactiveValuesToList(input)
    disable_inputs(input_list,F)
    
    # RUN GGIR ----------------------------------------------------------------
    # At the moment (2019-06-22) I use a for loop processing 1 file at a time. 
    # This way we can get the progress with every file being processed
    # Need to improve this in the future
    
    withProgress(message = 'Aggregating data', value = 0, {
      
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Processing files")
      # Run g.part2
      GGIR::g.shell.GGIR(mode=2,         
                         #BASIC SETTINGS
                         datadir=ggir_args$datadir(),
                         outputdir=ggir_args$outputdir(),
                         studyname = ggir_args$studyname(),
                         f0 = 1, f1 = length(ggir_args$datadir()),
                         overwrite = input$overwrite, 
                         # PRE-PROCESSING AND METRICS (g.part1 and g.part2 arguments):
                         windowsizes = ggir_args$windowsizes(), 
                         desiredtz = ggir_args$desiredtz(),   
                         # G.PART2
                         strategy = strategy, 
                         hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end,
                         maxdur = 0, ndayswindow = ndayswindow,
                         includedaycrit = input$includedaycrit, 
                         idloc = 2,
                         # MX metrics and Intensity gradient as in the paper by Rowlands et al.
                         qlevels = c(960/1440,              #M1/3 (8h)
                                     1320/1440, 1380/1440,  #M120, M60
                                     1410/1440, 1425/1440,  #M30, M15,
                                     1435/1440), #M5
                         iglevels = c(seq(0,4000,by=25),8000),
                         ilevels = c(seq(0,1000,by=10), 8000),
                         # MVPA
                         mvpathreshold = c(),
                         boutcriter = 0.8, bout.metric = 4, 
                         closedbout = FALSE,
                         # REPORTS
                         do.report=c(2),
                         visualreport=FALSE, 
                         do.parallel = FALSE)
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Done")
      disable_inputs(input_list,T)
    })
    
    # Read datasets for plots
    data_wk <<- read.csv(file.path(folder(), 
                                   grep("part2_summary", dir(folder()), value = TRUE)))
    data_day <<- read.csv(file.path(folder(),
                                    grep("part2_daysummary", dir(folder()), value = TRUE)))
    
    # PLOTS -------------------------------------------------------------------
    # Person to plot
    output$person2plot = renderUI({
      div(
        pickerInput(inputId = ns("person"),
                    label = "Select the person to plot",
                    choices = basename(data_wk$filename),
                    selected = basename(data_wk$filename)[1],
                    options = list(`live-search` = TRUE))
      )
    })
    
    # Acc metric to plot
    output$acc2plot = renderUI({
      path = file.path(ggir_args$metadatadir(), "meta", "ms2.out")
      files = dir(path)
      file2load = dir(path)[amatch(input$person, dir(path), maxDist = 6)]
      file2load = file.path(path, file2load)
      env <- reactiveFileReader(intervalMillis = 1000, session = session,
                                filePath = file2load, readFunc = LoadToEnvironment)
      IMP <<- env()[['IMP']]
      
      div(
        pickerInput(inputId = ns("acc"),
                    label = "Select the acceleration metric to plot",
                    choices = colnames(IMP$metashort)[2:ncol(IMP$metashort)],
                    selected = colnames(IMP$metashort)[2],
                    options = list(`live-search` = TRUE))
      )
    })
    
    # Plots
    # MX METRICS
    output$plot_mx = renderPlotly({
      row = amatch(input$person, basename(data_wk$filename))
      vars = grep(paste("_", input$acc, "_", sep = ""), colnames(data_wk), value = TRUE)
      vars = c(grep("AD_p66", vars, value = TRUE),
               grep("AD_p91", vars, value = TRUE),
               grep("AD_p95", vars, value = TRUE),
               grep("AD_p97", vars, value = TRUE),
               grep("AD_p98", vars, value = TRUE),
               grep("AD_p99", vars, value = TRUE))
      nums = as.numeric(data_wk[row, vars])
      
      p <- plot_ly(
        type = 'scatterpolar',
        r = nums,
        theta = c('M1/3','M120','M60', 'M30', 'M15', 'M5'),
        fill = 'toself'
      )
      p <- layout(p, polar = list(radialaxis = list(visible = T,
                                                    range = c(0,1000))),
                  showlegend = F)
      
      p
    })
    
    # ILEVELS
    output$plot_ilevels = renderPlotly({
      row = amatch(input$person, basename(data_wk$filename))
      vars = grep(paste("_", input$acc, "_", sep = ""), colnames(data_wk), value = TRUE)
      vars = vars[grep("AD_.0.10", vars):grep("AD_.990.", vars)]
      nums = as.numeric(sort(data_wk[row, vars], decreasing = TRUE))
      
      p <- plot_ly(
        x = seq(0,1000,by=10),
        y = nums,
        name = "Time in intensity levels",
        type = "bar"
      )
      p <- layout(p, yaxis = list(type = "log"))
      p
    })
  })
  
  
  
  
  # GO TO CHECK DATA QUALITY ------------------------------------------------
  output$gosleep = renderUI({
    actionBttn(inputId = ns("gosleep"), label = "Check sleep data",
               style = "jelly", color = "danger", size = "sm", block = TRUE)
  })
  
  observeEvent(input$gosleep,{
    updateTabsetPanel(parent_session, "navbarpage", selected = "sleep")
  })
  
  
  # RETURN ------------------------------------------------------------------
  return(list(
    strategy = reactive(strategy),
    hrs.del.start = reactive(hrs.del.start), 
    hrs.del.end = reactive(hrs.del.end), 
    ndayswindow = reactive(ndayswindow),
    includedaycrit = reactive(input$includedaycrit)
  ))
}