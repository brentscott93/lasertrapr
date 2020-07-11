#' clean_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shiny NS tagList 
mod_clean_data_ui <- function(id){
  ns <- NS(id)
  tagList(
   
    fluidRow(
      box(width = 12, title = "Selected Folders",
          verbatimTextOutput(ns('selected_folders'))
      ),
      # box(width = 2, title = 'Load Data',
      #        actionButton(ns('graph'), 'Graph', width = '100%')
      # )
    ),
    
    fluidRow(
      box(width = 9, title = "Graph Options",
          fluidRow(
            column(5,
                   
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("mode"),
                     label = 'View Mode',
                     choices = c("Raw" = "raw",
                                 "Detrend" = "detrend"),
                     direction = "horizontal",
                     width = "100%",
                     justified = TRUE,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: black"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: black"))
                   )
      
                   
            ), #col close
          
            column(2, 
                   textInput(ns('mv2nm'), 
                             'nm/mV', 
                             value = 1,
                             placeholder = 'Enter numeric conversion',
                             width = '100%')
            ),
            column(2,
                 
                    # h5("File Markers"),
                   div( style = 'margin-top: 30px;',
                   shinyWidgets::materialSwitch(
                     inputId = ns("hide_markers"),
                     label = "Markers", 
                     value = TRUE,
                     status = "primary"
                   )
                   )
                     # shinyWidgets::radioGroupButtons(
                     #   inputId = ns("hide_markers"),
                     #   label = 'File Markers',
                     #   choices = c("Show" = "show",
                     #               "Hide" = "hide"),
                     #   direction = "horizontal",
                     #   width = "100%",
                     #   justified = TRUE,
                     #   checkIcon = list(
                     #     yes = tags$i(class = "fa fa-check-square",
                     #                  style = "color: black"),
                     #     no = tags$i(class = "fa fa-square-o",
                     #                 style = "color: black"))
                     # )
                     ),
                   column(3,
                          
                          actionButton(ns('graph'), 'Graph', width = '100%',
                                       style="color: #fff; background-color: #05a029; margin-top: 25px;")
                   ),
                     
                  # ) #conditional close
          ),
          fluidRow(
            column(12, 
                 
                uiOutput(ns("trap_filter")),
            )
            # column(2, 
            #        div( style = 'margin-top: 50px;',
            #        shinyWidgets::materialSwitch(
            #          inputId = ns("filter_status"),
            #          label = "Filter", 
            #          value = FALSE,
            #          status = "primary"
            #        )
          )
                   # shinyWidgets::radioGroupButtons(
                   #   inputId = ns("filter_status"),
                   #   label = 'Filter Status',
                   #   choices = c("Off" = "off",
                   #               "On" = "on"),
                   #   direction = "horizontal",
                   #   justified = TRUE,
                   #   checkIcon = list(
                   #     yes = tags$i(class = "fa fa-check-square",
                   #                  style = "color: black"),
                   #     no = tags$i(class = "fa fa-square-o",
                   #                 style = "color: black"))
                   # )
          #  )
                   
          #)#row close
      ), #boxclose
      
      
      box(title = "Cleaning Tools", width = 3,
          fluidRow(
            column(12,
                   
                   # conditionalPanel(
                   #   condition = " input.trap_obs_selectInput != null &&
                   #                  input.trap_obs_selectInput.length > 0 ", ns = ns,
                     
                     textOutput(ns("move_files")),
                     actionButton(ns("trap_move_sheets_actionButton"),
                                  "Move",
                                  icon=icon("suitcase"),
                                  width = "100%"),
                     
                     
                  # ) #conditional close
            ) #col close
          ) ,
          br(),
          fluidRow(
            column(12,
                   # conditionalPanel(
                   #   condition = " input.trap_obs_selectInput != null &&
                   #                  input.trap_obs_selectInput.length > 0", ns = ns,
                     
                     textOutput(ns("trim_files")),
                     actionButton(ns("trap_trim_dygraph_actionButton"),
                                  "Cut",
                                  icon = icon("cut"),
                                  width = "100%")
                     
                  # ) #conditional close
                   
            )#col close
          )#row close
          
          
          
      ) #ox close
    ),#row close
    
    
    box(title = "Data Trace", width = 12,
    fluidRow(column(12,
 
               dygraphs::dygraphOutput(ns("dygraph_clean")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"),
  
      ))), #col, row, box close
    
        fluidRow(
          tabBox(width = 8, 
                 side = 'right',
            title = "Remove Baseline",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "baseline_mode", 
            tabPanel("Range", 
                     fluidRow(column(3, actionButton(ns('baseline_graph_range'), 'Baseline Range', width = '100%'))),
                     fluidRow(column(12, 
                                     plotOutput(ns('range'))  %>% 
                                       shinycssloaders::withSpinner(type = 8, color = "#373B38"),
                                     verbatimTextOutput(ns('range_mean'))
                     ))),#tabPanel close
            tabPanel("MV", 
                     fluidRow(column(3,actionButton(ns('baseline_graph_mv'), 'Baseline MV', width = '100%'))),
                     fluidRow(
                     column(6, 
                           plotOutput(ns('mv'), brush = ns('mv_brush'))  %>% 
                              shinycssloaders::withSpinner(type = 8, color = "#373B38"),
                     ), #col close
                     column(6, 
                           # actionButton(ns('measure'), 'Calculate average of selection', width = '100%'),
                            plotOutput(ns('baseline_histo')) %>% 
                              shinycssloaders::withSpinner(type = 8, color = "#373B38"),
                           
                     ) #col close
                     ), #row close
                   
                     fluidRow(
                       column(12,
                       verbatimTextOutput(ns('baseline_avg'))
                       )
                     )#rowclose
            )
           
          ), #tabBox close
                
        
  
  box(width = 4, title = "Save Processed Data",
      fluidRow(
        column(12, 
               shinyWidgets::radioGroupButtons(
                 inputId = ns("how_to_process"),
                 label = "How do you want to process this obs?",
                 choices = c('Detrend' = 'detrend'),
                 selected = 'detrend',
                 justified = T,
                 checkIcon = list(
                   yes = tags$i(class = "fa fa-check-square", 
                                style = "color: black"),
                   no = tags$i(class = "fa fa-square-o", 
                               style = "color: black"))
               ),
               
               shinyWidgets::radioGroupButtons(
                 inputId = ns("include"),
                 label = "Do you want to include this obs in analysis?",
                 choices = c('No', 'Yes'),
                 justified = T,
                 checkIcon = list(
                   yes = tags$i(class = "fa fa-check-square", 
                                style = "color: black"),
                   no = tags$i(class = "fa fa-square-o", 
                               style = "color: black"))
               ),
               verbatimTextOutput(ns('current_mv2nm')),
               actionButton(ns('save'), 'Save', width = '100%',  
                            style="color: #fff; background-color: #b80f07; margin-top: 25px;")
       
        ) # col close
      ) # row close
  ) #box close
  
  
  ), #rowclose
   fluidRow( 
  box(width = 12, title = "Status Table",
      column(12,
     gt::gt_output(ns('status'))
      )
  ),
  )
 
  )
  
}
    
#' clean_data Server Function
#' @import tidyverse
#' @importFrom magrittr "%<>%"
#' @noRd 

mod_clean_data_server <- function(input, output, session, f){
  ns <- session$ns
  
  observeEvent(f$obs_input, ignoreNULL = T, ignoreInit = T, {
    req(input$graph > 0)
    showNotification('Switched to new observation. Please update graph and baseline measures to proceed.', 
                     type = 'warning')
    
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "mode",
      choices = c("Raw" = "raw",
                  "Detrend" = "detrend"),
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: black"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: black"))
    )
    
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "how_to_process",
      choices = c( "Detrend" = "detrend"),
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: black"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: black"))
    )
    
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "include",
      choices = c('No', 'Yes'),
      selected = 'No',
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square", 
                     style = "color: black"),
        no = tags$i(class = "fa fa-square-o", 
                    style = "color: black"))
    )
    
    # shinyWidgets::updateMaterialSwitch(session = session, 
    #                                     inputId = 'filter_status',
    #                                     value = FALSE)
    
    # shinyWidgets::updateMaterialSwitch(session = session, 
    #                                     inputId = 'hide_markers',
    #                                     value = FALSE)
    # 
      #shinyjs::hide('clean_col')
    # shinyjs::hide('mv')
    # shinyjs::hide('range')
    
  })
  
 observe({ golem::print_dev(f$project_ns) })
 
  output$selected_folders <- renderPrint({
    
    validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select folders'))
   
    cat('Project:', f$project$name, ' | Conditions:', f$conditions$name, ' | Date:', f$date$name, ' | Observation:', f$obs$name)
    
      })
  
  rv <- reactiveValues(wait = FALSE)
  #### slect obs ####
  #OBS
  #1 list of obs names
  
  
  #files
  #tibble of file names
  trap_files <- reactive({
    list_dir(f$obs$path) %>%
      dplyr::filter(str_detect(name, "Data"))
  })
  
  #END obtain filenames/paths for trap file selectors
  
  #------------------------------------------------------------------------------------------------------------
  #Start prepare/clean data

  
  rv$update_graph <- 0
  #MOVE SHEETS to new obs
  observeEvent(input$trap_move_sheets_actionButton, {
    showModal(modalDialog(
      tagList(
        h4("Select an option to continue.")
      ),
      title="Do you really want to move these file?",
      footer = tagList(actionButton(ns("confirm_trap_move_sheets_actionButton"), "Yes, move."),
                       modalButton("Cancel")
      )
    ))
  })
#   
 
  rv$move_trap <- 0
  observeEvent(input$confirm_trap_move_sheets_actionButton, {
    req(substring(f$obs_input, 1, 3) == 'obs')
    removeModal()
    all_obs <- list_dir(f$date$path, pattern = 'obs') %>% nrow
    move_obs(f = f,
             trap_selected_date = f$date$path,
             trap_obs = all_obs,
             trap_files = trap_files(),
             trap_selected_obs = f$obs$path,
             dygraph_clean_date_window_1 = input$dygraph_clean_date_window[[1]],
             dygraph_clean_date_window_2 = input$dygraph_clean_date_window[[2]])
    
    rv$update_filter <- rv$update_filter + 1
     f$current_obs <- f$obs$name
     f$new_obs <- f$new_obs + 1
    
  #  rv$move_trap <- rv$move_trap + 1
    
  })
  
  #the move obs will create a new folder and observatrion data
  #this will trigger theh obs selectInput to retrigger to update inlcuding the new folder and select the
  #current user selection
  #this will bounce back here and update the graph by simuating a click of the button
  observeEvent(f$new_obs_refresh_graph, ignoreNULL = T, ignoreInit = T, {
    shinyjs::click('graph')
  })

  observeEvent(f$obs_input, {
    req(substring(f$obs_input, 1, 3) == 'obs')
    trap_path <- list_files(f$obs$path, pattern = 'trap-data.rds')
   rv$filter_max <- nrow(readRDS(trap_path$path)$grouped[[1]])
   
  })
  
  observeEvent(rv$update_filter, {
   
    trap_path <- list_files(f$obs$path, pattern = 'trap-data.rds')
    rv$filter_max <- nrow(readRDS(trap_path$path)$grouped[[1]])
    
  })
 
  
  # observeEvent(f$re, {
  #   req(substring(f$obs_input, 1, 3) == 'obs')
  #   trap_path <- list_files(f$obs$path, pattern = 'trap-data.rds')
  #   rv$filter_max <- nrow(readRDS(trap_path$path)$grouped[[1]])
  #   
  # })
  output$trap_filter <- renderUI({
    req(substring(f$obs_input, 1, 3) == 'obs')
    
    
    sliderInput(ns("trap_filter_sliderInput"),
                label = "Filter large dataset",
                value = c(1, rv$filter_max),
                min = 1,
                max = rv$filter_max,
                ticks = F,
                width = "100%")
    
  })




  observeEvent(f$obs_input, ignoreInit = T, {
    req(substring(f$obs_input, 1, 3) == 'obs')
    rv$update_graph <-  rv$update_graph + 1
  })
  #dygraph
  data <- reactiveValues()
  
  observeEvent(input$graph, ignoreInit = T, {
    if(substring(f$obs_input, 1, 3) != 'obs'){
      showNotification('No observation folder selected.', type = 'error')
    }
  })
  dg_data <- eventReactive(input$graph,  {
    
    if(substring(f$obs_input, 1, 3) != 'obs') showNotification('No obs selected', type = 'error')
    req(substring(f$obs_input, 1, 3) == 'obs')
    
    a <- attempt::attempt(as.numeric(input$mv2nm))
    if(attempt::is_try_error(a)) showNotification('mv to nm converions not a number', type = 'error')
    req(!attempt::is_try_error(a))
   
      current_obs <- f$obs$path
      #rv$current_graph_obs <- f$obs$name
      trap_data <- list_files(current_obs) %>%
        dplyr::filter(str_detect(name, "trap-data")) %>%
        dplyr::pull(path)

      data <- readRDS(trap_data) %>%
        tidyr::unnest(cols = c(grouped)) %>%
        dplyr::mutate(seconds = 1:nrow(.)/5000,
                      bead = bead*as.numeric(input$mv2nm)) %>%
        dplyr::select(seconds, bead)

      # if(input$filter_status == FALSE){
      #   data
      # } else {
         f1 <- input$trap_filter_sliderInput[[1]]
         f2 <-  input$trap_filter_sliderInput[[2]]
         data %<>% slice(f1:f2)
     # }
      
  })

   

  output$dygraph_clean <- dygraphs::renderDygraph({
    req(f$obs_input, dg_data(), input$graph > 0)# data <- dg_data()
    if(isolate(input$mode) == 'raw'){
      
      data <- dg_data()
      
    } else if(isolate(input$mode) == 'detrend'){
      
      break_pts <- seq(25000, nrow(dg_data()), by = 25000)
      data <- dg_data() %>% 
        mutate(bead = as.vector(pracma::detrend(bead, tt = "linear", bp = break_pts)))
      
    } else if(isolate(input$mode) == 'range'){
      
      data <- dg_data() %>% 
        mutate(bead = bead - base$range)
      
    } else if(isolate(input$mode) == 'mv'){
      data <- dg_data() %>% 
        mutate(bead = bead - base$baseline_fit$estimate[1])
    }
              
    
    number_files <- nrow(data)/25000
    
    end_file <- seq(5, by = 5, length.out = number_files)
    
    
    if(isolate(input$hide_markers) == TRUE){
      
      dygraphs::dygraph(data,  ylab = "mV", xlab = "Seconds",  main = isolate(f$obs$name)) %>%
        dygraphs::dySeries("bead", color = "black") %>%
        dygraphs::dyRangeSelector(fillColor ="", strokeColor = "black") %>%
        add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
        dygraphs::dyUnzoom() %>%
        dygraphs::dyOptions(axisLabelColor = "black",
                            gridLineColor = "black",
                            axisLineColor = "black",
                            axisLineWidth = 3,
                            axisLabelFontSize = 15,
                            drawGrid = FALSE)
      
    } else {
      
      dygraphs::dygraph(data,  ylab = "mV", xlab = "Seconds",  main = isolate(f$obs$name)) %>%
        dygraphs::dySeries("bead", color = "black") %>%
        dygraphs::dyRangeSelector(fillColor ="", strokeColor = "black") %>%
        # add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
        dygraphs::dyUnzoom() %>%
        dygraphs::dyOptions(axisLabelColor = "black",
                            gridLineColor = "black",
                            axisLineColor = "black",
                            axisLineWidth = 3,
                            axisLabelFontSize = 15,
                            drawGrid = FALSE)
      
      
    }
    
    
  })
# 
#
#   #########
#
  move_from_index <- reactive({
    req(trap_files())
    start_of_file_indices <- seq(0,
                                 by = 5,
                                 length.out = nrow(trap_files()))

    move_files_from <- round_any(input$dygraph_clean_date_window[[1]],
                                 5,
                                 f = floor)

    from_index <- which(start_of_file_indices == move_files_from)
    return(from_index)

  })

  move_to_index <- reactive({
    end_of_file_indices <- seq(5,
                               by = 5,
                               length.out = nrow(trap_files()))

    move_files_to <- round_any(input$dygraph_clean_date_window[[2]],
                               5,
                               f = ceiling)

    to_index <- which(end_of_file_indices == move_files_to)
    return(to_index)
  })

  #######
  output$move_files <- renderText({
    validate(need(input$dygraph_clean_date_window[[1]], 'Please load data to clean'))
    paste0("Move Files ",
           move_from_index(),
           " to ",
           move_to_index()
    )

  })
# #   
  trim_from <- reactive({
    try(round_any(input$dygraph_clean_date_window[[1]], 0.0002, f = round))
  })

  trim_to <- reactive({

    try(round_any(input$dygraph_clean_date_window[[2]], 0.0002, f = round))

  })
# 
  output$trim_files <- renderText({
    validate(need(trim_from(), 'Please load data to clean'))
    paste0("Delete data from ",
           trim_from(),
           "s",
           " to ",
           trim_to(),
           "s"
    )
  })

# #   
  # output$ <- dy_date_window_from <- renderText({
  #   paste0("From: ", input$dygraph_clean_date_window[[1]])
  # })
  # 
  # output$dy_date_window_to <- renderText({
  #   paste0("To: ", input$dygraph_clean_date_window[[2]])
  # })

  #trim sheets data
  observeEvent(input$trap_trim_dygraph_actionButton, {
    showModal(modalDialog(
      tagList(
        h4("This will delete the selected data.")
      ),
      title="Do you really want to ERASE the selection?",
      footer = tagList(actionButton(ns("confirm_trap_trim_dygraph_actionButton"), "Yes, cut."),
                       modalButton("Cancel")
      )
    ))
  })
# #   
  observeEvent(input$confirm_trap_trim_dygraph_actionButton, {
    removeModal()
        trim_obs(trap_selected_obs = f$obs$path,
                 trap_grouped_file = dg_data(),
                 trim_from = trim_from(),
                 trim_to = trim_to(),
                 f = f)
    rv$update_filter <- rv$update_filter + 1
    showNotification("Data trimmed. Graph will refresh.")
    shinyjs::click('graph')
  })
#   
#  
#  
#  
#   

  #### Process Data ####
  # create mv 
  
    
  base_mv_graph <- eventReactive(input$baseline_graph_mv, {
    
    if(substring(f$obs_input, 1, 3) != 'obs') showNotification('No obs selected', type = 'error')
    req(substring(f$obs_input, 1, 3) == 'obs')
    
    if(is_empty(dg_data())) showNotification('Graph obs before continuing', type = 'error')
    req(dg_data())
    #shinyjs::show('mv')
    base$mv_df <- tibble(mean = RcppRoll::roll_mean(dg_data()$bead, n = 15, align = 'left', fill = NULL),
                    var = RcppRoll::roll_var(dg_data()$bead, n = 15, align = 'left', fill = NULL))
    
    ggplot(base$mv_df)+
      geom_hex(aes(mean, var), bins = 75)+
      ggtitle('Select area on plot to set baseline population')+
      ylab('Variance')+
      xlab('Mean')+
      scale_fill_gradient(low = 'green', high = 'red')+
      theme_classic(base_size = 12)+
      theme(panel.background = element_rect(fill = 'black'),
            legend.position = 'none')
    
    
  })
      
    base_range_graph <- eventReactive(input$baseline_graph_range, {
      req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) < 10)
      #shinyjs::show('range')
      range_df <- dg_data() %>% 
        dplyr::filter(between(seconds, as.numeric(trim_from()), as.numeric(trim_to())))
      
      base$range <- mean(range_df$bead)
      
      ggplot(range_df)+
        geom_line(aes(x = seconds, y = bead))+
        geom_hline(yintercept = base$range, color = 'red', size = 2)+
        ylab('mV')+
        xlab('Seconds')+
        ggtitle('Baseline range selected with mean')+
        theme_classic(base_size = 14)
  })
  
    observe({
     req( input$dygraph_clean_date_window[[1]], input$dygraph_clean_date_window[[2]])
      golem::print_dev(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]])) })
    observeEvent(input$baseline_graph_range, {
      if(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) > 10){
        showNotification('Baseline range selection too long. Make a selection less than 10 seconds.', type = 'error')
      }
      })
      
    observeEvent(input$baseline_graph_range, {
      req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) < 10)
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "mode",
        choices = c("Raw" = "raw",
                    "Detrend" = "detrend",
                    "Remove base" = "range"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: black"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: black"))
      )
      
      #update saving options
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "how_to_process",
        choices = c("Detrend" = "detrend",
                    "Remove base" = "remove_base"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: black"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: black"))
      )
      
      
    })
       
    observeEvent(input$baseline_graph_mv, {
      
      
      #update saving options
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "how_to_process",
        choices = c("Detrend" = "detrend",
                    "Remove MV" = "remove_mv"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: black"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: black"))
      )
      
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "mode",
        choices = c("Raw" = "raw",
                    "Detrend" = "detrend",
                    "Remove MV" = "mv"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: black"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: black"))
      )
      
    })
    
      
  output$range_mean <- renderPrint({
    validate(need(base$range, 'Press button to calculate mean of selected range'))
    cat('The selected baseline range has a mean of ', base$range, ' mV')
  })
      
  output$range <- renderPlot({
    req(is.ggplot(base_range_graph()))
    req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) < 10)
    base_range_graph()
  })
  output$mv <- renderPlot({
    req(is.ggplot(base_mv_graph()))
    base_mv_graph()
  })
  
 
  
  base <- reactiveValues(done = 0)
  observe({
    req(input$mv_brush)
    #baseline_pop <- input$mv_brush
    mv_df  <- base$mv_df 
    #baseline data and fit to density fit
    baseline <- dplyr::filter(mv_df, dplyr::between(mean, input$mv_brush$xmin, input$mv_brush$xmax) & dplyr::between(var, input$mv_brush$ymin, input$mv_brush$ymax))
    baseline_fit <- MASS::fitdistr(baseline$mean, 'normal')
    
    
    #return values to reactive list
    base$baseline <- baseline
    base$baseline_fit <- baseline_fit
    
    
   # showNotification('Baseline population fit.')
  })
  
  output$baseline_histo <- renderPlot({
    req(not_null(base$baseline), not_null(base$baseline_fit))
    hist(base$baseline$mean, pch=20, breaks=25, prob=T, main="Baseline Population", xlab = 'Displacement (nm)')
    curve(dnorm(x, base$baseline_fit$estimate[1], base$baseline_fit$estimate[2]), col="red", lwd=2, add=T)
    
  })
  
  output$baseline_avg <- renderPrint({
    validate(need(base$baseline_fit$estimate[1], 'Baseline MV not measured'))
    cat('Baseline average = ', base$baseline_fit$estimate[1], 'mV')
  })
  
  logger <- reactiveValues()
  status <- reactiveValues()
  observeEvent(input$save, ignoreInit = T, {
    if(substring(f$obs_input, 1, 3) != 'obs') showNotification('No obs selected', type = 'error')
    req(substring(f$obs_input, 1, 3) == 'obs')
    if(!is.numeric(try(as.numeric(input$mv2nm)))) showNotification('mv to nm converions not a number', type = 'error')
    req(is.numeric(try(as.numeric(input$mv2nm))))
    withProgress(message = 'Saving Data', {
      current_obs <- f$obs$path
      
      trap_data <- list_files(current_obs) %>%
        dplyr::filter(str_detect(name, "trap-data")) %>%
        dplyr::pull(path)
      
       data <- readRDS(trap_data)
       
      
       grouped <- data %>% 
         tidyr::unnest(cols = c(grouped)) %>%
         dplyr::select(bead, trap) %>% 
         dplyr::mutate(bead = bead*as.numeric(input$mv2nm))
       
      setProgress(0.3)
      
     if(input$how_to_process == 'detrend'){
       
       break_pts <- seq(25000, nrow(dg_data()), by = 25000)
       processed <- grouped %>% 
         mutate(bead = as.vector(pracma::detrend(bead, tt = "linear", bp = break_pts)))
       
     } else if(input$how_to_process == 'remove_base'){
       
       processed <- grouped %>% 
         mutate(bead = bead - base$range)
       
     } else if(input$how_to_process == 'remove_mv'){
       processed <- grouped %>% 
         mutate(bead = bead - base$baseline_fit$estimate[1])
     }
     
    processed %<>% mutate(bead = bead * as.numeric(input$mv2nm))
      
     data  %<>% mutate(processed = list(processed),
                       processed_how = input$how_to_process, 
                       mv2nm = as.numeric(input$mv2nm),
                       include = input$include,
                       status = 'processed')
     
     setProgress(0.5)
     
     saveRDS(data, file = file.path(f$obs$path, 'trap-data.rds'))
     
     setProgress(0.75)
      # index <- input$save
      # golem::print_dev(paste('index is-', index))
     # logger[[as.character(input$save)]] <- paste(f$conditions$name, f$obs$name, 'processed and saved at', Sys.time(), '\n',
     #                          'Type of processing:', input$how_to_process, '\n',
     #                          'Include:', input$include, '\n \n')
     
     golem::print_dev( logger[[as.character(input$save)]] )
     all_trap_paths <- list_files(f$date$path, pattern = 'trap-data.rds', recursive = T)
     
     setProgress(0.9)
     status$df<- map_df(all_trap_paths$path, readRDS) %>% 
       dplyr::select(project, conditions, date, folder, processed_how, mv2nm, include, status)
     
     setProgress(1)
   
    })
    showNotification(paste(f$conditions$name, f$obs$name, 'successfully processed and saved.'), type = 'message')
  })
  
 # output$log <- renderPrint({
 #  #cat(logger[[as.character(input$save)]])
 #   cat(unlist(isolate(reactiveValuesToList(logger))))
 # })
 
 output$current_mv2nm <- renderPrint({
   cat('mV to nm conversion: ', input$mv2nm)
 })
 
  output$status <- gt::render_gt(width = gt::pct(100),{
    req(status$df)
    status$df %>% 
      rename('Project' = project, 
             'Conditions' = conditions,
             'Date' = date, 
             'Folder' = folder,
             'Processor' = processed_how,
             'mV-to-nm' = mv2nm,
             'Include' = include, 
             'Status' = status) %>% 
      gt::gt(rowname_col = 'Folder') %>% 
      gt::tab_header(
        title = "Current Status of Observations",
        subtitle = "Green indicates ready"
      ) %>% 
      gt::tab_stubhead(label = "folder") %>%
      gt:: tab_spanner(
        label = "Description",
        columns = vars(Project, Conditions, Date)
      ) %>% 
      gt::tab_spanner(
        label = "Progress Indicators",
        columns = vars(Processor, `mV-to-nm`, Include, Status)
      ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "red"),
        locations = gt::cells_body(
          rows = Status == 'grouped')
      ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "green"),
        locations = gt::cells_body(
          rows = Status == 'processed'))
      
  })
  #when obs folder switches reset all outputs  widgets
  # observeEvent(f$obs_input, {
  # output$mv <- renderPlot({})
  # output$baseline_histo <- renderPlot({})
  # output$baseline_avg <- renderPrint({})
  # #output$dygraph_clean <- dygraphs::renderDygraph({})
  # output$range_mean <- renderPrint({})
  # output$range <- renderPlot({})
  # shinyjs::reset('mode')
  # })
  
  
  #End prepare / clean data
 
}
    
## To be copied in the UI
# mod_clean_data_ui("clean_data_ui")
    
## To be copied in the server
# callModule(mod_clean_data_server, "clean_data_ui")
 
