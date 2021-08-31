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
   
    # fluidRow(
    #   box(width = 12, title = "Selected Folders",
    #       verbatimTextOutput(ns('selected_folders'))
    #   ),
    #   # box(width = 2, title = 'Load Data',
    #   #        actionButton(ns('graph'), 'Graph', width = '100%')
    #   # )
    # ),
    
    fluidRow(
      box(width = 9, title = "Graph Options", 
          fluidRow(
            column(6,
                   
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
          
            column(3, 
                   numericInput(ns('mv2nm'), 
                             'Step Cal (nm/mV)', 
                             value = 1,
                             #placeholder = 'Enter numeric conversion',
                             width = '100%')
            ),
            # column(2,
            #      
            #         # h5("File Markers"),
            #        div( style = 'margin-top: 30px;',
            #        shinyWidgets::materialSwitch(
            #          inputId = ns("hide_markers"),
            #          label = "Markers", 
            #          value = TRUE,
            #          status = "primary"
            #        )
            #        )
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
                    # ),
                   column(3,
                          
                          actionButton(ns('graph'), 
                                       'Graph',
                                       width = '100%',
                                       icon = icon('chart-line'),
                                       style="margin-top: 25px;")
                   ),
                     
                  # ) #conditional close
          ),
          fluidRow(
            column(12, 
                 #shinyWidgets::setSliderColor("#F012BE", 1),
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
                     
                     #textOutput(ns("move_files")),
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
    
    fluidRow(
    box(title = "Data Trace", width = 12,
    fluidRow(column(12,
 
               dygraphs::dygraphOutput(ns("dygraph_clean")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"),
  
      )))), #col, row, box close
    
        fluidRow(
          tabBox(id = ns('baseline_tab_box'), width = 8, 
                 side = 'right',
            title = "Remove Baseline",
            # The id lets us use input$tabset1 on the server to find the current tab
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
               textInput(ns('nm2pn'), 
                         'Trap Stiffness (pN/nm)',
                         placeholder = 'Equipartition Value'),
               verbatimTextOutput(ns('current_mv2nm')),
               actionButton(ns('save'), 
                            'Save',
                            width = '100%',   
                            icon = icon('save'),
                            style="margin-top: 25px;")
       
        ) # col close
      ) # row close
  ) #box close
  
  
  ), #rowclose
   fluidRow( 
  box(width = 12, title = "Status Table",
      column(12,
             actionButton(ns('status_graph'), 'Update Info table'),
     DT::DTOutput(ns('info')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
      )
  ),
  )
 
  )
  
}
    
#' clean_data Server Function
#' @import tidyverse hexbin
#' @importFrom magrittr "%<>%"
#' @noRd 

mod_clean_data_server <- function(input, output, session, f){
  ns <- session$ns
  # observe({
  #   req(input$baseline_tab_box)
  #   if(input$baseline_tab_box == 'Range'){
  #     shinyjs::show('range')
  #     shinyjs::hide('mv')
  #   } else {
  #     shinyjs::show('mv')
  #     shinyjs::hide('range')
  #   }
  # })
  observeEvent(f$obs_input, ignoreNULL = T, ignoreInit = T, {
    req(input$graph > 0)
    showNotification('Switched obs', 
                     type = 'message',
                     duration = 2)
    
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
     
    
    
   
    shinyjs::hide('dygraph_clean')

    # if(input$baseline_tab_box == 'Range'){
    #   shinyjs::hide('range')
    # } else {
    #   shinyjs::hide('mv')
    # }
    # base$range_df <- NULL
     base$show_range <- NA
     base$range <- NA
     base$baseline_fit$estimate[1] <- NA
     base$show_mv  <- NA
    # base$baseline_fit <- NULL
    # base$mv_df <- NULL
  })
  
 observe({ golem::print_dev(f$project_ns) })
 
  output$selected_folders <- renderPrint({
    
    validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select folders'))
   
    cat('Project:', f$project$name, ' | Conditions:', f$conditions$name, ' | Date:', f$date$name, ' | Observation:', f$obs$name)
    
      })
  
  rv <- reactiveValues(wait = FALSE, update_filter = 0)
  #### slect obs ####
  #OBS
  #1 list of obs names
  
  
  #files
  #tibble of file names
  trap_files <- reactive({
    list_files(f$obs$path) %>%
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
    all_obs <- list_dir(f$date$path) %>% 
      dplyr::filter(str_detect(name, 'obs')) %>% 
      nrow
    
    move_obs(trap_selected_date = f$date$path,
             trap_selected_obs = f$obs$path,
             trim_from = trim_from(),
             trim_to = trim_to(),
             f = f,
             trap_obs = all_obs,
             hz = hz())
             # trap_files = trap_files(),
             # trap_selected_obs = f$obs$path,
             # dygraph_clean_date_window_1 = input$dygraph_clean_date_window[[1]],
             # dygraph_clean_date_window_2 = input$dygraph_clean_date_window[[2]])
    
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
    trap_path <- list_files(f$obs$path, pattern = 'trap-data.csv')
   rv$filter_max <- nrow(data.table::fread(trap_path$path, select = "raw_bead"))
   
  })
  
  observeEvent(rv$update_filter, ignoreInit = T, {
   
    trap_path <- list_files(f$obs$path, pattern = 'trap-data.csv')
    rv$filter_max <- nrow(data.table::fread(trap_path$path, select = "raw_bead"))
    
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

  
 
  
  dg_data <- reactiveValues(make_graph = 0)
  observeEvent(input$graph,  {
    
    defend_if_empty(f$obs_input,
                    ui = 'No observation folder selected.', 
                    type = 'error')
    
    defend_if_not_equal(substring(f$obs_input, 1, 3), 'obs', 
                        ui = 'No observation folder selected.', type = 'error')
   
    
    # a <- attempt::attempt(as.numeric(input$mv2nm))
    # if(attempt::is_try_error(a)) showNotification('mv to nm converions not a number', type = 'error')
    # req(!attempt::is_try_error(a))
   
      current_obs <- f$obs$path
      #rv$current_graph_obs <- f$obs$name
      trap_data <- list_files(current_obs) %>%
        dplyr::filter(str_detect(name, "trap-data.csv")) %>%
        dplyr::pull(path)

      data <- data.table::fread(trap_data, sep = ",") %>%
        dplyr::mutate(bead = raw_bead*as.numeric(input$mv2nm)) %>%
        dplyr::select(time_sec, bead)

      # if(input$filter_status == FALSE){
      #   data
      # } else {
         f1 <- input$trap_filter_sliderInput[[1]]
         f2 <-  input$trap_filter_sliderInput[[2]]
         #dygraph kept refreshing on change file
         #but only the title was changing and data wasnt
         #this will keep the dygraph from refreshing until input$graph is clicked again
         dg_data$title <- f$obs$name
         dg_data$data <- data %<>% slice(f1:f2) 
         dg_data$make_graph <- dg_data$make_graph + 1
         shinyjs::show('dygraph_clean')
        
         
  })

  trap_data_trace <- eventReactive(dg_data$make_graph, ignoreNULL = T, ignoreInit = T, {
    
    if(isolate(input$mode) == 'raw'){
      
      data <- dg_data$data
      
    } else if(isolate(input$mode) == 'detrend'){
      
      break_pts <- seq(hz()*5, nrow(dg_data$data), by = hz()*5)
      data <- dg_data$data %>% 
        mutate(bead = as.vector(pracma::detrend(bead, tt = "linear", bp = break_pts)))
      
    } else if(isolate(input$mode) == 'range'){
      
      data <- dg_data$data %>% 
        mutate(bead = bead - base$range)
      
    } else if(isolate(input$mode) == 'mv'){
      data <- dg_data$data %>% 
        mutate(bead = bead - base$baseline_fit$estimate[1])
    }
    
    if(isolate(input$mv2nm) == 1){
      
      dg <- dygraphs::dygraph(data,  ylab = "mV", xlab = "Seconds",  main = dg_data$title) %>%
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
      
    } else {
      dg <- dygraphs::dygraph(data,  ylab = "nm", xlab = "Seconds",  main = dg_data$title) %>%
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
      
      
    #}
     
  })

  output$dygraph_clean <- dygraphs::renderDygraph({
    req(trap_data_trace())
    trap_data_trace()
  })

output$move_files <- renderText({
  validate(need(trim_from(), 'Please load data to clean'))
  paste0("Move data from ",
         trim_from(),
         "s",
         " to ",
         trim_to(),
         "s"
  )
})

  hz <- reactive({
    req(f$obs$path)
    o <- list.files(path = f$obs$path,
                    pattern = "options.csv",
                    full.names = TRUE)
    as.integer(data.table::fread(o, select = "hz")$hz)
  })
  trim_from <- reactive({
    try(as.numeric(round_any(input$dygraph_clean_date_window[[1]], 1/hz(), f = round)))
  })

  trim_to <- reactive({

    try(as.numeric(round_any(input$dygraph_clean_date_window[[2]], 1/hz(), f = round)))

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
                 trim_from = trim_from(),
                 trim_to = trim_to(),
                 f = f, 
                 hz = hz())
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
  
    observeEvent(input$baseline_graph_mv, {
      base$show_mv <- 'yes'
      #shinyjs::show('mv')
})
  base_mv_graph <- eventReactive(input$baseline_graph_mv, {
    defend_if_empty(input$dygraph_clean_date_window[[1]],
                    ui = 'Graph/Upload data before calculating baseline',
                    type = 'error')
    defend_if_not_equal(substring(f$obs_input, 1, 3),
                         'obs',
                       ui = 'No obs selected', 
                       type = 'error' )
   
   defend_if_empty(dg_data$data, ui = 'Graph obs before continuing', type = 'error')
    
  
    base$mv_df <- tibble(mean = RcppRoll::roll_mean(dg_data$data$bead, n = 30, align = 'left', fill = NULL),
                    var = RcppRoll::roll_var(dg_data$data$bead, n = 30, align = 'left', fill = NULL))
    
    if(input$mv2nm <= 1) showNotification('Convert your data to nm before calculating MV.', type = 'error')
    req(input$mv2nm > 1)
   # colorz <- RColorBrewer::brewer.pal(8, 'RdPu')
    ggplot(base$mv_df)+
      geom_hex(aes(mean, var), bins = 75)+
      ggtitle('Select area on plot to set baseline population')+
      ylab('Variance')+
      xlab('Mean')+
      scale_fill_gradient(low = 'green', high = 'red')+
      theme_classic(base_size = 12)+
      theme(legend.position = 'none',
            panel.background = element_rect(colour = "black", size=2))
    
    
  })
      
 
  

  observeEvent(input$baseline_graph_range, {
    defend_if_empty(input$dygraph_clean_date_window[[1]],
                    ui = 'Graph/Upload data before calculating baseline',
                    type = 'error')
    defend_if_not_equal(substring(f$obs_input, 1, 3),
                        'obs',
                        ui = 'No obs selected', 
                        type = 'error' )

    a <- attempt::attempt(is.numeric(input$dygraph_clean_date_window[[1]]))
    defend_if(attempt::is_try_error(a), ui =  showNotification('Load data before calculating baseline range'), type = 'error')
  
    if(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) > 10){
      showNotification('Baseline range selection too long. Make a selection less than 10 seconds.', type = 'error')
    }
    req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) <= 10)
    #browser()
   
     if(var(dg_data$data$bead) == 1) showNotification('Current mV-to-nm is 1. Do you need to enter a conversion value?', type = 'warning')
    # req(var(dg_data$data$bead) > 5)
    base$range_df <- dg_data$data %>% 
      dplyr::filter(between(seconds, as.numeric(trim_from()), as.numeric(trim_to())))
    
    base$range <- mean(base$range_df$bead)
    
    base$range_update_graph <-  base$range_update_graph + 1
    base$show_range <- 'yes'
    #shinyjs::show('range')

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
  #   base_range_graph <- eventReactive(base$range_update_graph, ignoreInit = T, {
  #     
  #     req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) <= 10)
  #     req(base$range_df)
  #    # browser()
  #     p <- ggplot(base$range_df)+
  #       geom_line(aes(x = seconds, y = bead))+
  #       geom_hline(yintercept = base$range, color = 'red', size = 2)+
  #       ylab('mV')+
  #       xlab('Seconds')+
  #       ggtitle('Baseline range selected with mean')+
  #       theme_classic(base_size = 14)
  # })
  
    # observe({
    #  req( input$dygraph_clean_date_window[[1]], input$dygraph_clean_date_window[[2]])
    #   golem::print_dev(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]))
    #   })
    # 
      
   
       
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
    cat('The selected baseline range has a mean of ', base$range, ' nm')
  })
      
  ggrange <- eventReactive(base$range_update_graph, {
    req(base$range_df)
    
    ggplot(isolate(base$range_df))+
      geom_line(aes(x = seconds, y = bead), color = 'black')+
      geom_hline(yintercept = isolate(base$range), color = 'firebrick', size = 2)+
      ylab('nm')+
      xlab('Seconds')+
      ggtitle('Baseline range selected with mean')+
      theme_classic(base_size = 16)#+
      #theme(panel.background = element_rect(colour = "black", size=2))
  })
  
  output$range <- renderPlot({
    req(!is.na(base$show_range))
    req(base$range_df, base$range)
    
    #req(length(input$dygraph_clean_date_window[[1]]:input$dygraph_clean_date_window[[2]]) <= 10)
    ggrange()
     
  })
  output$mv <- renderPlot({
    req(!is.na(base$show_mv))
    req(is.ggplot(base_mv_graph()))
    base_mv_graph()
  })
  
 
  
  base <- reactiveValues(done = 0, range_update_graph = 0, show_range = NA, show_mv = NA)
  observe({
    #req(!is.na(base$show_mv))
    req(input$mv_brush)
    #baseline_pop <- input$mv_brush
    mv_df  <- base$mv_df 
    #baseline data and fit to density fit
    baseline <- dplyr::filter(mv_df, dplyr::between(mean, input$mv_brush$xmin, input$mv_brush$xmax) & dplyr::between(var, input$mv_brush$ymin, input$mv_brush$ymax))
    req(!is_empty(baseline$mean))
    baseline_fit <- MASS::fitdistr(baseline$mean, 'normal')
    #return values to reactive list
    base$baseline <- baseline
    base$baseline_fit <- baseline_fit
   # showNotification('Baseline population fit.')
  })
  
  output$baseline_histo <- renderPlot({
    req(not_null(base$baseline), not_null(base$baseline_fit))
    req(base$baseline_fit$estimate[1])
    
    hist(base$baseline$mean, 
         pch=20, 
         breaks=25,
         prob=T, 
         main="Baseline Population", 
         xlab = 'Nanometers')
         # col.axis = "white", 
         # col.lab = 'white',
         # col.main = 'white',
         # col =trap_gray(), 
         # border =  'black')
    curve(dnorm(x, base$baseline_fit$estimate[1], base$baseline_fit$estimate[2]), 
          col='firebrick', lwd=2, add=T)
    #graphics::box()
    
  })
  
  output$baseline_avg <- renderPrint({
    validate(need(base$baseline_fit$estimate[1], 'Baseline MV not measured'))
    cat('Baseline average = ', base$baseline_fit$estimate[1], 'mV')
  })
  
  logger <- reactiveValues()
  status <- reactiveValues()
  observeEvent(input$save, ignoreInit = T, {
    defend_if_not_equal(substring(f$obs_input, 1, 3),
                        'obs',
                        'No obs selected', type = 'error')
    
    #test that mv to nm is a number
    defend_if_blank(input$mv2nm, ui = 'Enter step cal', type = 'error')

    mv2nm_test <- attempt::attempt(as.numeric(input$mv2nm))
    if(attempt::is_try_error(mv2nm_test)) showNotification('nm to pN converion not a number', type = 'error')
    req(!attempt::is_try_error(mv2nm_test))
    
    #test that mv to nm is a number
    defend_if_blank(input$nm2pn, ui = 'Enter trap stiffness', type = 'error')
   
    nm2pn_test <- attempt::attempt(as.numeric(input$nm2pn))
    if(attempt::is_try_error(nm2pn_test)) showNotification('nm to pN converion not a number', type = 'error')
    req(!attempt::is_try_error(nm2pn_test))
    
    withProgress(message = 'Saving Data', {
      current_obs <- f$obs$path
      
      trap_data <- list_files(current_obs) %>%
        dplyr::filter(str_detect(name, "trap-data.csv")) %>%
        dplyr::pull(path)
      
       data <- data.table::fread(trap_data, sep = ",") %>% 
                dplyr::mutate(processed_bead = raw_bead*as.numeric(input$mv2nm))
       
      setProgress(0.3)
      
     if(input$how_to_process == 'detrend'){
       
       break_pts <- seq(hz()*5, nrow(dg_data$data), by = hz()*5)
       
          data %<>% dplyr::mutate(processed_bead = as.vector(pracma::detrend(processed_bead, tt = "linear", bp = break_pts)))
       
     } else if(input$how_to_process == 'remove_base'){
       
         data %<>% dplyr::mutate(processed_bead = processed_bead - base$range)
       
     } else if(input$how_to_process == 'remove_mv'){
       
        data %<>% dplyr::mutate(processed_bead = processed_bead - base$baseline_fit$estimate[1])
     }
      
      if(input$include == 'No'){
         input_include <- FALSE
       } else {
         input_include <- TRUE
       }
      
      o <- list.files(path = f$obs$path, 
                      pattern = "options.csv",
                      full.names = TRUE)
      o <- fread(o)
      
      o %<>%  dplyr::mutate(processor = input$how_to_process, 
                            mv2nm = as.numeric(input$mv2nm),
                            nm2pn = as.numeric(input$nm2pn),
                            include = input_include)
      
     # data  %<>% dplyr::mutate(processor = input$how_to_process, 
     #                   mv2nm = as.numeric(input$mv2nm),
     #                   nm2pn = as.numeric(input$nm2pn),
     #                   include = input_include)
     
     setProgress(0.5)
     
     data.table::fwrite(data, file = file.path(f$obs$path, 'trap-data.csv'), sep = ",")
     data.table::fwrite(o, file = file.path(f$obs$path, 'options.csv'), sep = ",")
     
     setProgress(0.75)
      # index <- input$save
      # golem::print_dev(paste('index is-', index))
     # logger[[as.character(input$save)]] <- paste(f$conditions$name, f$obs$name, 'processed and saved at', Sys.time(), '\n',
     #                          'Type of processing:', input$how_to_process, '\n',
     #                          'Include:', input$include, '\n \n')
     
  
     golem::print_dev( logger[[as.character(input$save)]] )
     all_trap_paths <- list_files(f$date$path, pattern = 'options.csv', recursive = T)
     
     setProgress(0.9)
    
     status$df <- map_df(all_trap_paths$path, ~data.table::fread(., 
                                                                 sep = ",",
                                                                 select = c("obs", "processor", "mv2nm", "nm2pn", "include"),
                                                                 nrows = 1))
     
     
     setProgress(1)
   
    })
    showNotification(paste(f$conditions$name, f$obs$name, 'successfully processed and saved.'), type = 'message')
  })
  
  observeEvent(input$status_graph, {
    defend_if_null(f$date_input, ui = 'Whoops. You forgot to select a date folder.', type = 'error')
    defend_if_blank(f$date_input, ui = 'Whoops. You forgot to select a date folder.', type = 'error')
    
    all_trap_paths <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
    defend_if_empty(all_trap_paths, ui = "No trap-data.rds files in date folder yet. Start by loading date with 'Initialize Data'",  type = 'error')
    golem::print_dev(all_trap_paths$path)
    status$df <- map_df(all_trap_paths$path, ~data.table::fread(.,
                                                                sep = ",",
                                                                select = c("obs", "processor", "mv2nm", "nm2pn", "include"),
                                                                nrows = 1))
    
    # df %<>% map(~summarize(., obs = unique(obs),
    #                           processor = unique(processor), mv2nm, nm2pn, include))

                                  
      #dplyr::select(obs, processor, mv2nm, nm2pn, include)
    showNotification('Status table refreshed', type = 'message')
  })
 # output$log <- renderPrint({
 #  #cat(logger[[as.character(input$save)]])
 #   cat(unlist(isolate(reactiveValuesToList(logger))))
 # })
 
 output$current_mv2nm <- renderPrint({
   cat('mV to nm conversion: ', input$mv2nm)
 })
 
  output$info <- DT::renderDT({
    req(status$df)
    status$df %>% 
      rename('Obs' = obs,
             'Processor' = processor,
             'mV-to-nm' = mv2nm,
             'nm-to-pN' = nm2pn,
             'Include' = include) %>% 
      DT::datatable() %>% 
      DT::formatStyle('Include', 
                      color = DT::styleEqual(c(F, T), c('red', 'black'))
      )
      # ) %>%
      # DT::formatStyle('Report', 
      #                 color = DT::styleEqual(c('error', 'success'), c('red', 'black'))
      # ) %>%
      # DT::formatStyle('Review', 
      #                 color = DT::styleEqual(c(NA, F, T), c('grey', 'red', 'green'))
      # )
      # gt::gt(rowname_col = 'Obs') %>% 
      # gt::tab_stubhead('Obs') %>% 
      # gt::tab_header(
      #   title = "Current Status of Observations",
      #   subtitle = "Green indicates ready"
      # )  %>%
      # gt:: tab_spanner(
      #   label = "Description",
      #   columns = vars(Project, Conditions, Date)
      # ) %>% 
      # gt::tab_spanner(
      #   label = "Progress Indicators",
      #   columns = vars(Processor, `mV-to-nm`, `nm-to-pN`, Include, Status)
      # ) %>% 
      # gt::tab_style(
      #   style = gt::cell_fill(color = purple()),
      #   locations = gt::cells_body(
      #     rows = Status == 'grouped')
      # ) %>% 
      # gt::tab_style(
      #   style = gt::cell_fill(color = pink()),
      #   locations = gt::cells_body(
      #     rows = Status == 'processed'))
      
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
 
