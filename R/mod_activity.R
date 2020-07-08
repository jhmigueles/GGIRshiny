#' activity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param ggir_args GGIR arguments specified in mod_processing1 (datadir, outputdir, metadatadir,..).
#' @param ggir_args2 GGIR arguments specified in mod_distribution (strategy, maxdur,...).
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_activity_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        div(
          fluidRow(
            col_4(
              numericInput(ns("thLIG"), label = "Threshold for sedentary/light",
                           value = 40)
            ),
            col_4(
              numericInput(ns("thMOD"), label = "Threshold for light/moderate",
                           value = 100)
            ),
            col_4(
              numericInput(ns("thVIG"), label = "Threshold for moderate/vigorous",
                           value = 400)
            )
          )
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
            actionButton(inputId = ns("run4"), icon = icon("play"),
                         label = "Run",
                         width = "100%")
          ),
          col_6(
            uiOutput(ns("filesummary"))
          )
        )
      ),
      mainPanel(
        fluidRow(
          col_6(
            uiOutput(ns("person2plot"))
          ),
          col_6(
            uiOutput(ns("mvpa2plot"))
          )
        ),
        fluidRow(
          col_6(
            plotlyOutput(ns("plot_mvpa"))
          ),
          col_6(
            uiOutput(ns("ageGroup")),
            uiOutput(ns("recommendations"))
          )
        )
      )
    )
  )
}

#' activity Server Function
#'
#' @noRd 
mod_activity_server <- function(input, output, session,ggir_args, ggir_args2){
  ns <- session$ns
  
  # RUN G.PART5 -----------------------------------------------------------
  observeEvent(input$run4,{
    # Disable inputs while running
    input_list <- reactiveValuesToList(input)
    disable_inputs(input_list,F)
    
    # RUN GGIR ----------------------------------------------------------------
    withProgress(message = 'Deriving physical activity data', value = 0, {
      
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Processing files")
      # Run g.part5
      GGIR::g.shell.GGIR(mode=5,         
                         #BASIC SETTINGS
                         datadir=ggir_args$datadir(),
                         outputdir=ggir_args$outputdir(),
                         metadatadir = ggir_args$metadatadir(),
                         studyname = ggir_args$studyname(),
                         f0 = 1, f1 = length(ggir_args$datadir()),
                         overwrite = input$overwrite, 
                         # PRE-PROCESSING AND METRICS (g.part1 and g.part2 arguments):
                         windowsizes = ggir_args$windowsizes(), 
                         desiredtz = ggir_args$desiredtz(),
                         # G.PART5 arguments:
                         excludefirstlast.part5 = FALSE,
                         maxdur = 0,  
                         threshold.lig = input$thLIG, threshold.mod = input$thMOD, threshold.vig = input$thVIG,
                         boutdur.mvpa = c(1,5,10), boutdur.in = c(10,20,30), boutdur.lig = c(1,5,10),
                         boutcriter.mvpa = 0.8, boutcriter.in = 0.9, boutcriter.lig = 0.8, 
                         timewindow = "MM",
                         save_ms5rawlevels=TRUE, save_ms5raw_without_invalid=FALSE,
                         save_ms5raw_format="csv",
                         # REPORTS
                         do.report=c(5),
                         visualreport=TRUE, 
                         do.parallel = FALSE)
      incProgress(0.5/length(ggir_args$datadir()), 
                  detail = "Done")
      disable_inputs(input_list,T)
    })
    # Person to plot
    output$person2plot = renderUI({
      path = file.path(ggir_args$metadatadir(), "meta", "ms5.out")
      files = reactive(dir(path))
      div(
        pickerInput(inputId = ns("person"),
                    label = "Select the person to plot",
                    choices = files(),
                    selected = files()[1],
                    options = list(`live-search` = TRUE))
      )
    })
    
    folder = reactive(file.path(ggir_args$metadatadir(), "results"))
    file = reactive(grep("part5_daysummary", dir(folder()), value = T))
    data = reactive(read.csv(file.path(folder(), file())))
    
    output$mvpa2plot = renderUI({
      div(
        pickerInput(inputId = ns("mvpa2plot"),
                    label = "Select the person to plot",
                    choices = grep("dur_day_MVPA", colnames(data()), value = TRUE),
                    selected = grep("dur_day_MVPA", colnames(data()), value = TRUE)[1],
                    options = list(`live-search` = TRUE))
      )
    })
    # Plot
    output$plot_mvpa = renderPlotly({
      nums = data()[which(data()$filename == input$person), input$mvpa2plot]
      plot_ly(
        x = data()[which(data()$filename == input$person), "weekday"],
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
      actionBttn(ns("recs"), label = "Reach physical activity recommendations?",
                 style = "jelly", color = "danger", size = "sm", block = TRUE)
    })
    
    # See file summary
    output$filesummary = renderUI({
      actionBttn(ns("filesummary"), label = "PDF report",
                 style = "jelly", color = "danger", size = "sm", block = TRUE)
    })
  })
  
  # Meet recommendations? ---------------------------------------------------
  observeEvent(input$recs,{
    folder = file.path(ggir_args$metadatadir(), "results")
    file = grep("part5_personsummary", dir(folder), value = T)
    data = read.csv(file.path(folder, file))
    var = paste(input$mvpa2plot, "_wei", sep = "")
    meanMVPA = (data[which(data$filename == input$person), var])
    
    threshold = ifelse(input$ageGroup == "Preschooler", 180,
                       ifelse(input$ageGroup == "Child", 60, 
                              ifelse(input$ageGroup == "Adolescent", 60, 21.5)))
    
    if(meanMVPA >= threshold){
      sendSweetAlert(
        session = session,
        title = "Yes!",
        text = "This participant is active",
        type = "success",
        btn_labels = "Ok",
        btn_colors = "#3085d6"
      )
    } else if(meanMVPA < threshold){
      sendSweetAlert(
        session = session,
        title = "No!",
        text = "This participant is inactive",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6"
      )
    }
  })
  
  
  # Open PDF report ---------------------------------------------------------
  observeEvent(input$recs,{
    folder = file.path(ggir_args$metadatadir(), "results", "file summary reports")
    file = reactive(dir(folder)[amatch(paste("Report", input$person, sep = "_"), dir(folder), maxDist = 6)])
    filepath = reactive(file.path(folder, file()))
    file.show(filepath())
  })
  
}