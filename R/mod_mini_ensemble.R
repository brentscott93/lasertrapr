#' mini_ensemble UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mini_ensemble_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, title = "Selected Folders",
          verbatimTextOutput(ns('selected_folders'))
      )),
    fluidRow(
      column(4, 
             
             box(width = NULL, title = 'Mini-Ensemble Analyzer Controls',
                 shinyWidgets::radioGroupButtons(
                   inputId = ns('which_obs'),
                   label = "Analyze which obs?",
                   choices = c("All" = "all",
                               "Single Obs" = "single"),
                   justified = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-check-square",
                                  style = "color: black"),
                     no = tags$i(class = "fa fa-square-o",
                                 style = "color: black"))
                 ),
                 
                 #conditionalPanel(conditions = "input.which_obs == 'single
                 
                 shinyWidgets::materialSwitch(ns('advanced'), 
                                              'Advanced',
                                              value = FALSE, 
                                              status = 'primary'),
                 sliderInput(ns('w_width'), 
                             label = 'Running Mean Window Width (ms)',
                             value = 50, 
                             min = 10, 
                             max = 300, 
                             step = 10),
                 
                 sliderInput(ns('displacement_threshold'), 
                             label = 'Set Displacement Threshold (ms)',
                             value = 8, 
                             min = 1, 
                             max = 10, 
                             step = 1),
                 
                 sliderInput(ns('time_threshold_ms'), 
                             label = 'Set Minimum Time Threshold (ms)',
                             value = 10, 
                             min = 1, 
                             max = 20, 
                             step = 1),
                 
                 numericInput(inputId = ns('hz'),
                              label = 'Sampling Frequency (Hz)', 
                              value = 5000),
                 
    
                 
                 actionButton(inputId = ns("analyze_trap"),
                              label = "Run Analysis",
                              icon = icon("running"),
                              width = "100%",
                              style = 'margin-top: 25px; color = #fff; background-color: #009BFF;')
                 
             ),
             
      ),
      
      
      column(8,
             box(width = NULL, title = 'Info',
                 actionButton(ns('info_table'), 'Refresh Info'),
                 DT::DTOutput(ns('table')) %>%  shinycssloaders::withSpinner(type = 8, color = "#373B38")
             ),
      ) # col close
    ),
    
    fluidRow(
      box(width = 12, title = 'Results', 
          background = 'black',  
          
          
          fluidRow(
            column(4, 
                   valueBoxOutput(ns('observation'), width = NULL)
            ),
            column(4, 
                   valueBoxOutput(ns('n_events'), width = NULL)
            ),
            column(4, 
                   valueBoxOutput(ns('seconds'), width = NULL)
            )), 
          fluidRow( 
            column(9, 
                   box(width = NULL, title = 'View Analysis',
                       plotOutput(ns('mini_dygraph')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
                   )
            ), 
            column(3, 
                   box(width = NULL, title = 'Review Analysis',
                       actionButton(ns('view_results'), 
                                    'View Results',
                                    width = '100%',
                                    icon = icon('area-chart'),
                                    style = 'margin-bottom: 25px;'),
                       shinyWidgets::radioGroupButtons(
                         inputId = ns('quality_control'),
                         label = "Should analysis be accepted?",
                         choices = c("No" = FALSE,
                                     "Yes" = TRUE),
                         justified = TRUE,
                         selected = FALSE,
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-check-square",
                                        style = "color: black"),
                           no = tags$i(class = "fa fa-square-o",
                                       style = "color: black"))
                       ),
                       actionButton(ns('save_review'), 
                                    'Save Review', 
                                    width = '100%',
                                    style = "color: #fff; background-color: #50D400; margin-top: 25px;")
                       
                   )
            ), #col close
          )#,
          # fluidRow(
          #        box(width = 4, title = 'HM-Model Summary',
          #            verbatimTextOutput(ns('hm_model_summary')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
          #        ),
          # box(width = 8, title = 'Real Time Event Frequency',
          #     plotOutput(ns('event_freq')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
          # )
          # ), #rowclose
          # 
          # fluidRow(
          #   box(width = 12, title = 'HM-Model Data (Running Mean & Variance)', collapsible = T, collapsed = T, 
          #       dygraphs::dygraphOutput(ns('hm_model_dygraph1'), height = "250px") %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"),
          #       dygraphs::dygraphOutput(ns('hm_model_dygraph2'), height = "250px") %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
          #   ),
          # 
          # ),
          # # fluidRow(
          # #   box(width = 12, title = 'Running Mean (HM-Model Data)',
          # #      
          # #   ),
          #   
          # #),
          # fluidRow(
          #   column(12,
          #          box(width = NULL, title = 'Analyzed Data', collapsible = T, collapsed = T, 
          #              dygraphs::dygraphOutput(ns('overlay_dygraph'), height = '400px') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
          #          )
          #   )
          # ) #rowclose
      ) #results box close
    )
  )#taglist clost
}
    
#' mini_ensemble Server Function
#'
#' @noRd 
mod_mini_ensemble_server <- function(input, output, session){
  ns <- session$ns
  
  
  hm <- reactiveValues(analyze = 0)
  
  observeEvent(input$analyze_trap, {
    golem::print_dev(input$which_obs)
    defend_if_empty(f$date, ui = "Please select a folder", type = 'error')
    defend_if_blank(f$date_input, ui = 'Select a date', type = 'error')
    
    if(input$which_obs == 'single'){
      defend_if_empty(f$obs_input, ui = 'Select an obs', type = 'error')
      defend_if_not_equal(substring(f$obs_input, 1, 3), 'obs', ui = 'Select an obs', type = 'error')
    }
    showNotification('Analysis will begin shortly...', type = 'message', duration = 2)
    hm$analyze <-   hm$analyze + 1
    
  })
  
  observeEvent(hm$analyze, ignoreInit = T, {
    
    if(input$which_obs =='single'){
      file <- list_files(f$obs$path, pattern = 'trap-data.csv')
      trap_data <- vroom::vroom(file$path)
    } else {
      files <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
      trap_data <- purrr::map(files$path, vroom::vroom)
    }
    
    # if(c('run_mean_overlay', 'rescaled_mini_data' %not_in% colnames(trap_data)){
    #   trap_data %<>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead)) 
    # } else {
    #   trap_data %<>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead, run_mean_overlay)) 
    # }
    
    
    if(is.null(input$hz)){
      hz <- 5000
    } else {
      hz <- input$hz
    }
    
    if(is.null(input$w_width)){
      w_width <- 50
    } else {
      w_width <- input$w_width
    }
    
    if(is.null(input$displacement_threshold)){
      displacement_threshold <- 50
    } else {
      displacement_threshold <- input$w_width
    }
    
    if(is.null(input$time_threshold_ms)){
      time_threshold_ms <- 50
    } else {
      time_threshold_ms <- input$time_threshold_ms
    }
    
    withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
      purrr::walk(trap_data, ~mini_ensemble_analyzer(trap_data = .x,
                                                     w_width_ms = w_width,
                                                     hz = hz, 
                                                     displacement_threshold =  displacement_threshold , 
                                                     time_threshold_ms = time_threshold_ms)
      )
    })
    
    shinyWidgets::sendSweetAlert(session = session,
                                 title =  "Mini-Ensemble Analysis Complete",
                                 text = "Results saved to 'lasertrapr' folder",
                                 type = "success")
    shinyjs::click('info_table')
  })
  
  
  #### SAVE REVIEW ####
  
  observeEvent(input$save_review, {
    allow_if('obs_input' %in% names(f), ui = 'Select an obs', type = 'error')
    defend_if_blank(f$obs_input, ui = 'Select an obs', type = 'error')
    
    withProgress(message = 'Saving Review', {
      
      td <- list_files(f$obs$path, pattern = 'trap-data.csv')
      trap <- vroom::vroom(td$path)
      setProgress(0.7)
      trap_reviewed <- trap %>% 
        dplyr::mutate(review = input$quality_control)
      
      vroom::vroom_write(trap_reviewed, file = file.path(f$obs$path, 'trap-data.csv'))
      setProgress(1, detail = 'Done')
    })
    showNotification('Review saved' , type = 'message')
    shinyjs::click('info_table')
  })
  
  info <- eventReactive(input$info_table, {
    defend_if_empty(f$date, ui = 'Please select a date folder', type = 'error')
    showNotification('Refreshing table', type = 'message')
    path <- file.path('~','lasertrapr', f$project$name, f$conditions$name, f$date$name)
    files <- list_files(path, pattern = 'trap-data.csv', recursive = T)
    trap_data <- purrr::map(files$path, vroom::vroom)
    # if('hm_overlay' %not_in% colnames(trap_data)){
    #   t <- trap_data  %>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead)) 
    # } else {
    #   t <- trap_data  %>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead, hm_overlay)) 
    # }
    
    purrr::map_dfr(trap_data, ~function(trap_data){
                                   tibble::tibble(obs = unique(trap_data$obs), 
                                                  include = unique(trap_data$include),
                                                  analyzer = unique(trap_data$analyzer),
                                                  report = unique(trap_data$report),
                                                  review = unique(trap_data$review))
                                  }
    )
  
  })
  
  output$table <- DT::renderDT({
    req(info())
    info() %>% 
      dplyr::rename('Obs' = obs,
                    'Include' = include, 
                    'Analyzer' = analyzer,
                    #'Status' = status,
                    'Report' = report,
                    'Review' = review) %>% 
      DT::datatable() %>% 
      DT::formatStyle('Include', 
                      color = DT::styleEqual(c(F, T), c('red', 'black'))
      ) %>%
      DT::formatStyle('Report', 
                      color = DT::styleEqual(c('error', 'success'), c('red', 'black'))
      ) %>%
      DT::formatStyle('Review', 
                      color = DT::styleEqual(c(NA, F, T), c('grey', 'red', 'green'))
      )
  })
  
  
  
  
  observe({
    if(input$advanced){
      shinyjs::show('w_width')
      shinyjs::show('hz')
      shinyjs::show('displacement_threshold')
      shinyjs::show('time_threshold_ms')
      
    } else {
      shinyjs::hide('w_width')
      shinyjs::hide('hz')
      shinyjs::hide('displacement_threshold')
      shinyjs::hide('time_threshold_ms')
    }
  })
 
}
    
## To be copied in the UI
# mod_mini_ensemble_ui("mini_ensemble_ui_1")
    
## To be copied in the server
# callModule(mod_mini_ensemble_server, "mini_ensemble_ui_1")
 
