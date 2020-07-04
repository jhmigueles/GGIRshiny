#' processing1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param parent_session Parent session.
#'
#' @noRd 
#'
#' @import GGIR
#' @import ggplot2
#' @importFrom shiny NS tagList withProgress
#' @importFrom plotly renderPlotly plotlyOutput plot_ly add_segments rangeslider layout
#' @importFrom lubridate hour minute second
#' @importFrom tcltk tk_choose.files tk_choose.dir 
#' @importFrom shinyjs disabled useShinyjs enable disable
#' @importFrom shinyWidgets prettyToggle multiInput sliderTextInput pickerInput actionBttn
mod_processing1_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        div(
          actionButton(inputId = ns("datadir"), 
                       label = "Select the files to process",
                       width = "100%")
        ),
        br(),
        div(
          # fluidRow(
            multiInput(
              inputId = ns("accmetrics"),
              label = "Metrics to calculate:", 
              choiceNames = list("Angle X","Angle Y","Angle Z",
                                 "BFEN","BF X","BF Y","BF Z",
                                 "Dev median X","Dev median Y","Dev median Z",
                                 "EN","ENMO","ENMOa","HFEN","HFEN+","HF X","HF Y","HF Z",
                                 "LFEN","LFENMO","LF X","LF Y","LF Z",
                                 "MAD","Median X","Median Y","Median Z"),
              choiceValues = list("anglex","angley","anglez",
                                  "bfen","bfx","bfy","bfz",
                                  "dev_roll_med_acc_x","dev_roll_med_acc_y","dev_roll_med_acc_z",
                                  "en","enmo","enmoa","hfen","hfenplus","hfx","hfy","hfz",
                                  "lfen","lfenmo","lfx","lfy","lfz",
                                  "mad","roll_med_acc_x","roll_med_acc_y","roll_med_acc_z"),
              selected = c("enmo", "anglez")
            )
          # )
        ),
        br(),
        div(
          sliderTextInput(inputId = ns("epoch"),label = "Select the epoch length",
                          choices = c(1,seq(5,60,by = 5)), selected = 5, grid = TRUE,
                          post = " s")
        ),
        br(),
        div(
          pickerInput(inputId = ns("desiredtz"),
                      label = "Select the timezone where data was collected:", 
                      choices = as.character(OlsonNames()), selected = "Europe/London",
                      options = list(`live-search` = TRUE))
        ),
        br(),
        div(
          textInput(inputId = ns("studyname"),
                    label = "Study name:", 
                    value = "mystudy")
        ),
        br(),
        div(
          actionButton(inputId = ns("outputdir"), 
                       label = "Select directory to save output",
                       width = "100%")
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
            disabled(
              actionButton(inputId = ns("run1"), icon = icon("play"),
                           label = "Run",
                           width = "100%")
            )
          ),
          col_6(
            uiOutput(ns("godistribution"))
          )
        )
      ),
      mainPanel(
        fluidRow(
          col_6(
            div(style = 'overflow-x: scroll; overflow-y: scroll;height:300px;',
                tableOutput(ns("selected")), 
                width = 6, height = "150px")
          ),
          col_6(
            div(style = 'overflow-x: scroll; overflow-y: scroll;height:300px;',
                tableOutput(ns("outdir")), 
                width = 6, height = "300px")
          )
        ),
        fluidRow(
          br(),
          col_6(
            div(
              uiOutput(ns("person2plot"))
            )
          ),
          col_6(
            div(
              uiOutput(ns("acc2plot"))
            )
          )
        ),
        fluidRow(
          div(
            plotlyOutput(ns("plot"))
          )
        )
      )
    )
  )
}

#' processing1 Server Function
#'
#' @noRd 
mod_processing1_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  # Datadir -----------------------------------------------------------------
  datadir = NULL
  makeReactiveBinding("datadir")
  observeEvent(input$datadir,{
    # Reactive datadir
    datadir <<- tk_choose.files()
    # Enable run1
    if((length(datadir) > 0 & length(outputdir) > 0)) enable("run1")
  })
  # Show the selected files
  output$selected = renderTable({
    if(length(datadir) < 1) {
      data.frame(Files = "No selected files to process")
    } else {
      data.frame(Files = datadir)
    }
  })
  
  # Outputdir -----------------------------------------------------------------
  outputdir = NULL
  makeReactiveBinding("outputdir")
  observeEvent(input$outputdir,{
    # Define outputdir
    outputdir <<- tk_choose.dir()
    # Enable run1
    if((length(datadir) > 0 & length(outputdir) > 0)) enable("run1")
  })
  # Show the selected directory
  output$outdir = renderTable({
    if(length(outputdir) < 1) {
      data.frame(Directory = "No directory selected to save output")
    } else {
      data.frame(Directory = outputdir)
    }
  })
  
  # Outputdir -----------------------------------------------------------------
  metadatadir = reactive({file.path(outputdir, paste("output",input$studyname, sep = "_"))})
  
  # Run g.part1 -------------------------------------------------------------
  observeEvent(input$run1,{
    # Disable inputs while running
    input_list <- reactiveValuesToList(input)
    disable_inputs(input_list,F)
    
    # Metrics -----------------------------------------------------------------
    sel_acc = reactive(choose.metrics(input$accmetrics))
    
    # RUN GGIR ----------------------------------------------------------------
    
    # At the moment (2019-06-22) I use a for loop processing 1 file at a time. 
    # This way we can get the progress with every file being processed
    # Need to improve this in the future
    withProgress(message = 'Aggregating data', value = 0, {
      for (i in 1:length(datadir)) {
        fname = basename(datadir[i])
        incProgress(0.5/length(datadir), 
                    detail = paste("Processing \n", fname, sep = ""))
        # Run g.part1
        GGIR::g.part1(datadir=datadir[i],
                      outputdir=outputdir,
                      f0 = 1, f1 = length(datadir),
                      windowsizes = c(as.numeric(input$epoch),900,3600),
                      desiredtz = input$desiredtz,
                      studyname = input$studyname,
                      overwrite = input$overwrite,
                      do.enmo = sel_acc()$enmo, do.lfenmo = sel_acc()$lfenmo, do.en = sel_acc()$en,
                      do.bfen = sel_acc()$bfen, do.hfen=sel_acc()$hfen, do.hfenplus = sel_acc()$hfenplus,
                      do.mad = sel_acc()$mad, do.anglex=sel_acc()$anglex, do.angley=sel_acc()$angley,
                      do.anglez=sel_acc()$anglez, do.enmoa=sel_acc()$enmoa,
                      do.roll_med_acc_x=sel_acc()$roll_med_acc_x,
                      do.roll_med_acc_y=sel_acc()$roll_med_acc_y,
                      do.roll_med_acc_z=sel_acc()$roll_med_acc_z,
                      do.dev_roll_med_acc_x=sel_acc()$dev_roll_med_acc_x,
                      do.dev_roll_med_acc_y=sel_acc()$dev_roll_med_acc_y,
                      do.dev_roll_med_acc_z=sel_acc()$dev_roll_med_acc_z,
                      do.lfen = sel_acc()$lfen, do.lfx=sel_acc()$lfx,
                      do.lfy=sel_acc()$lfy, do.lfz=sel_acc()$lfz,
                      do.hfx=sel_acc()$hfx, do.hfy=sel_acc()$hfy, do.hfz=sel_acc()$hfz,
                      do.bfx=sel_acc()$bfx, do.bfy=sel_acc()$bfy, do.bfz=sel_acc()$bfz,
                      do.parallel=FALSE,   
                      # As default (listed here in case of future development)
                      do.cal = TRUE,lb = 0.2, hb = 15, n = 4,
                      spherecrit=0.3,minloadcrit=72,
                      printsummary=TRUE,print.filename=FALSE,
                      backup.cal.coef="retrieve",selectdaysfile=c(),
                      dayborder=0,dynrange=c(),
                      chunksize = 1)
        incProgress(0.5/length(datadir), 
                    detail = paste("Done \n", fname, sep = ""))
      }
      disable_inputs(input_list,T)
    })
    
    # Plot --------------------------------------------------------------------
    fnames = reactive({
      if(is.null(outputdir)) return()
      dir(file.path(metadatadir(),"meta", "basic"))
    })
    
    
    output$person2plot = renderUI({
      if(is.null(fnames())) return()
      div(
        pickerInput(inputId = ns("person"),
                    label = "Select the file to plot",
                    choices = fnames(), selected = fnames()[1],
                    options = list(`live-search` = TRUE))
      )
    })
    
    # metrics available
    data = NULL
    M = NULL
    makeReactiveBinding("data")
    makeReactiveBinding("M")
    output$acc2plot = renderUI({
      if(length(fnames()) == 0) return()
      file2load = input$person
      file2load = file.path(metadatadir(),"meta", "basic", file2load)
      env <- reactiveFileReader(intervalMillis = 1000, session = session,
                                filePath = file2load, readFunc = LoadToEnvironment)
      data <<- env()[['M']]$metashort
      M <<- env()[['M']]
      div(
        pickerInput(inputId = ns("acc"),
                    label = "Select the acceleration metric to plot",
                    choices = colnames(data)[2:ncol(data)], selected = colnames(data)[2],
                    options = list(`live-search` = TRUE))
      )
    })
    
    # Plot
    output$plot = renderPlotly({
      if(length(fnames()) == 0) return()
      data_plot = data.frame(time = GGIR::iso8601chartime2POSIX(data$timestamp, tz = input$desiredtz),
                             acc = data[,input$acc])
      
      p = plot_ly(x = data_plot$time, y = data_plot$acc, 
                  type = "scatter", mode = "lines")
      
      p = layout(p, yaxis = list(title = get_label.y(input$acc)))
      
      # NON-WEAR IN PLOT (TO DO)
      # nonwear = GGIR::g.weardec(M = M, wearthreshold = 2, ws2 = 900)
      # nonwear_blocks = GGIR::g.createcoordinates(r = nonwear$r1, timeline = 1:nrow(M$metalong))
      # clipping_blocks = GGIR::g.createcoordinates(nonwear$r3)
      # if(not_null(nonwear_blocks$x0)){
      #   for(i in 1:length(nonwear_blocks$x0)){
      #     p = add_
      #   }
      # }
      p
    })
  })
  
  
  # GO TO CHECK DATA QUALITY ------------------------------------------------
  output$godistribution = renderUI({
    actionBttn(inputId = ns("godistribution"), label = "Check acceleration distribution",
               style = "jelly", color = "danger", size = "sm", block = TRUE)
  })
  
  observeEvent(input$godistribution,{
    updateTabsetPanel(parent_session, "navbarpage", selected = "distribution")
  })
  
  
  # RETURN ------------------------------------------------------------------
  return(list(
    datadir = reactive(datadir),
    outputdir = reactive(outputdir),
    metadatadir = reactive(metadatadir()),
    windowsizes = reactive(c(as.numeric(input$epoch),900,3600)),
    desiredtz = reactive(input$desiredtz),
    studyname = reactive(input$studyname)
  ))
  
}

