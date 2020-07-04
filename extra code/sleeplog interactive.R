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
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom graphics rect
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
        div(
          uiOutput(ns("plot_log"))
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
          plotOutput(ns("plot_angle")
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
  
  # Data for plot
  IMP = reactive({
    path = file.path(ggir_args$metadatadir(), "meta", "ms2.out")
    files = dir(path)
    file2load = dir(path)[amatch(input$person, files, maxDist = 6)]
    file2load = file.path(path, file2load)
    env <- reactiveFileReader(intervalMillis = 1000, session = session,
                              filePath = file2load, readFunc = LoadToEnvironment)
    return(env()[['IMP']])
  })
  
  noons = reactive({
    noons = grep("12:00:00", IMP()$metashort$timestamp)
    if(noons[1] > 1) noons = c(1, noons)
    if(noons[length(noons)] < nrow(IMP()$metashort)) noons = c(noons, nrow(IMP()$metashort))
    return(noons)
  })
  
  ndays = reactive({length(noons()) - 1})
  
  # Plot angle to define sleeplog times
  output$plot_angle = renderPlot(height = reactive(ndays()*150),{
    # Refill first and last day
    angle = IMP()$metashort$anglez
    time = IMP()$metashort$timestamp
    epoch = ggir_args$windowsizes()[1]
    daylength = (60/epoch) * 1440
    # First day
    if(length(noons()[1]:noons()[2]) < (daylength + 1)){
      fill = (daylength + 1) - length(noons()[1]:noons()[2])
      angle = c(rep(NA, fill),angle)
      time = c(rep(NA, fill),time)
    }
    # Last day
    last = length(noons())
    if(length(noons()[last-1]:noons()[last]) < (daylength + 1)){
      fill = (daylength + 1) - length(noons()[last-1]:noons()[last])
      angle = c(angle, rep(NA, fill))
      time = c(time, rep(NA, fill))
    }
    
    # Redefine noons
    noons = grep("12:00:00", time)
    if(noons[1] > 1) noons = c(1, noons)
    if(noons[length(noons)] < length(time)) noons = c(noons, length(time))
    
    # Plot
    par(mfrow = c(ndays(), 1), mar = c(2,2,1,1))
    for(i in 1:ndays()){
      # Plot area
      d0 = noons[i]
      d1 = noons[i+1]
      plot(angle[d0:d1], type = "n", col = "blue", lwd = 2,
           xaxt = "n")
      # Grid and x axis
      at = seq(1, d1-d0+1, 6*60*60/epoch)
      abline(v = at, 
             lwd = 2, col = "lightgrey")
      axis(side = 1, at = at, labels = c("12:00", "18:00", "00:00", "06:00", "12:00"))
      # Angle line
      lines(angle[d0:d1], col = "blue", lwd = 2)
    }
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
        shinyWidgets::radioGroupButtons(
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
        shinyWidgets::radioGroupButtons(
          inputId = ns("sleeplog_coln1"),
          label = "Select the first sleep onset",
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
    output$sleeplog_nnights = renderUI({
      div(
        numericInput(
          inputId = ns("sleeplog_nnights"),
          label = "How many nights are in your sleep log?", value = 7, min = 1, step = 1
        )
      )
    })
    output$plot_log = renderUI({
      div(
        actionButton(inputId = ns("plot_log"), 
                     label = "Check sleep log times",
                     width = "100%")
      )
    })
  })  
  observeEvent(input$plot_log,{
    output$plot_angle = renderPlot(height = reactive(ndays()*150),{
      # Refill first and last day
      angle = IMP()$metashort$anglez
      time = IMP()$metashort$timestamp
      epoch = ggir_args$windowsizes()[1]
      daylength = (60/epoch) * 1440
      # First day
      if(length(noons()[1]:noons()[2]) < (daylength + 1)){
        fill = (daylength + 1) - length(noons()[1]:noons()[2])
        angle = c(rep(NA, fill),angle)
        time = c(rep(NA, fill),time)
      }
      # Last day
      last = length(noons())
      if(length(noons()[last-1]:noons()[last]) < (daylength + 1)){
        fill = (daylength + 1) - length(noons()[last-1]:noons()[last])
        angle = c(angle, rep(NA, fill))
        time = c(time, rep(NA, fill))
      }
      
      # Redefine noons
      noons = grep("12:00:00", time)
      if(noons[1] > 1) noons = c(1, noons)
      if(noons[length(noons)] < length(time)) noons = c(noons, length(time))
      
      # Load sleeplog
      log = GGIR::g.loadlog(loglocation = loglocation, colid = which(colnames(log_tmp) == input$sleeplog_colid),
                            coln1 = which(colnames(log_tmp) == input$sleeplog_coln1),
                            nnights = input$sleeplog_nnights,
                            sleeplogidnum = FALSE)
      
      log = log$sleeplog[which(log$sleeplog$ID == file),]
      
      # Plot
      par(mfrow = c(ndays(), 1), mar = c(2,2,1,1))
      for(i in 1:ndays()){
        # Plot area
        d0 = noons[i]
        d1 = noons[i+1]
        plot(angle[d0:d1], type = "n", col = "blue", lwd = 2,
             xaxt = "n")
        # Sleeplog SPT's
        rect(xleft = 1, ybottom = min(angle[d0:d1], na.rm = T), xright = 8000, ytop = max(angle[d0:d1], na.rm = T), 
             col = "azure2", lty = 0)
        # Grid and x axis
        at = seq(1, d1-d0+1, 6*60*60/epoch)
        abline(v = at, 
               lwd = 2, col = "lightgrey")
        axis(side = 1, at = at, labels = c("12:00", "18:00", "00:00", "06:00", "12:00"))
        
        # Angle line
        lines(angle[d0:d1], col = "blue", lwd = 2)
      }
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
      # Run g.part2
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
                         loglocation = "./",
                         colid = 1, coln1 = 9, nnights = 7,
                         do.visual=TRUE,
                         excludefirstlast = FALSE,
                         includenightcrit = 0,
                         # REPORTS
                         do.report=c(2),
                         visualreport=FALSE, 
                         do.parallel = FALSE)
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Done")
      disable_inputs(input_list,T)
    })
    
    # Save data for later use
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
    
    
    
    
    
    output$plot_sleep
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
  return(list(
    strategy = reactive(strategy),
    hrs.del.start = reactive(hrs.del.start), 
    hrs.del.end = reactive(hrs.del.end), 
    ndayswindow = reactive(ndayswindow),
    includedaycrit = reactive(input$includedaycrit)
  ))
}
