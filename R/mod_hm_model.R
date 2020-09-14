#' hm_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hm_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, title = "Selected Folders",
          verbatimTextOutput(ns('selected_folders'))
      )),
    fluidRow(
      column(4, 
       
             box(width = NULL, title = 'HM-Model/CP Analyzer Controls',
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
                             label = 'Window Width (dp)',
                             value = 150, 
                             min = 10, 
                             max = 300, 
                             step = 10),
                 
                 numericInput(inputId = ns('hz'),
                              label = 'Sampling Frequency (Hz)', 
                              value = 5000),
                 
                 
                shinyWidgets::prettyCheckbox(inputId = ns('em_random_start'),
                                              value = FALSE,
                                              label = "EM Random Start?", 
                                              status = "primary",
                                              shape = "curve",
                                              outline = TRUE),
                
                actionButton(inputId = ns("analyze_trap"),
                             label = "Run Analysis",
                             icon = icon("running"),
                             width = "100%",
                             style = 'margin-top: 25px;')
                                                          
                 ),
             
         ),
      
  
    column(8,
    box(width = NULL, title = 'Info Table',
        actionButton(ns('info_table'), 'Refresh Info'),
        DT::DTOutput(ns('table')) %>%  shinycssloaders::withSpinner(type = 8, color = "#373B38")
    ),
    ) # col close
    ),
    
    # fluidRow(
    #   box(width = 12, title = 'Results', 
    #   background = 'black',  
      
    tags$style(".small-box.bg-yellow { background-color: #1B9E77 !important; color: #f2f2f2 !important; }"),
          fluidRow(
            column(4, 
             valueBoxOutput(ns('observation'), width = NULL)
            ),
            column(4, 
             valueBoxOutput(ns('n_events'), width = NULL)
            ),
            column(4, 
             valueBoxOutput(ns('s2n'), width = NULL)
          )), 
      fluidRow( 
        column(9, 
             box(width = NULL, title = 'MV Plot',
                 plotOutput(ns('mv_by_state')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
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
                                icon = icon('save'),
                                style = "margin-top: 25px;")
                   
               )
        ), #col close
      ),
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
    fluidRow(
      column(12,
      box(width = NULL, title = 'Analyzed Data', collapsible = T, collapsed = T, 
             dygraphs::dygraphOutput(ns('overlay_dygraph'), height = '400px') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
    )
      )
    # ) #rowclose
    #   ) #results box close
    )
  )#taglist clost
}
    
#' hm_model Server Function
#' @importFrom magrittr "%<>%"
#' @param input,output,session,f module parameters
#' @noRd 
mod_hm_model_server <- function(input, output, session, f){
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
      trap_data <- purrr::map(file$path, vroom::vroom, delim = ",")
    } else {
      files <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
      trap_data <- purrr::map(files$path, vroom::vroom, delim = ",")
    }
    
    # if('hm_overlay' %not_in% colnames(trap_data)){
    #   trap_data %<>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead)) 
    # } else {
    #    trap_data %<>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead, hm_overlay)) 
    # }
    
    if(is.null(input$em_random_start)){
      em_start <- FALSE
    } else {
      em_start <- input$em_random_start
    }
    
    if(is.null(input$hz)){
      hz <- 5000
    } else {
      hz <- input$hz
    }
    
    if(is.null(input$w_width)){
      w_width <- 150
    } else {
      w_width <- input$w_width
    }
    
    # keep <- purrr::map_lgl(trap_data, ~unique(.$include) == T)
    # 
    # trap_data %<>% trap_data[keep]
    
    withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
    purrr::walk(trap_data, ~hidden_markov_changepoint_analysis(
                                    trap_data = .x,
                                     f = f,
                                     hz = hz,
                                     w_width = w_width,
                                     em_random_start = em_start, 
                                     is_shiny = T)
              )
       })
      
      shinyWidgets::sendSweetAlert(session = session,
                                   title =  "Hidden Markov Analysis Complete",
                                   text = "Results saved to 'lasertrapr' folder",
                                   type = "success")
     shinyjs::click('info_table')
  })
  
  
  trap_data <- eventReactive(input$view_results, {
    defend_if_empty(f$obs, ui = 'Please select an obs folder', type = 'error')
    defend_if_blank(f$obs_input, ui = 'Please select an obs folder', type = 'error')
  
    filenames <- c('trap-data.csv', 'measured-events.csv', 'hm-model-data.csv')
    paths <- map(filenames, ~list_files(f$obs$path, pattern = .x))
    data <-  map(paths, ~vroom::vroom(.x$path))
    names(data) <- c('trap', 'events', 'running')
    data
  })
  

  output$overlay_dygraph <- dygraphs::renderDygraph({
    req(trap_data())

    d <- data.frame(index = (1:nrow(trap_data()$trap)/5000),
                    raw = trap_data()$trap$processed_bead,
                    model = trap_data()$trap$hm_overlay)

    periods_df <- data.frame(start = trap_data()$events$cp_event_start_dp/5000,
                             stop = trap_data()$events$cp_event_stop_dp/5000,
                             keep = trap_data()$events$keep,
                             color = ifelse(trap_data()$events$keep == T, scales::alpha("#D95F02", 0.6), "#DEDEDE" )) 
    
    # events <- periods_df %>%  dplyr::filter(keep == T)
    # 
    # excluded_events <- periods_df %>% dplyr::filter(keep == F)
   # d <- vroom::vroom('~/lasertrapr/project_myoV-phosphate/myoV-S217A_pH-7.0_30mM-Pi/2019-02-27/obs-01/measured-events.csv')
   # excluded_events <-  d %>% dplyr::filter(keep == F)
     pni <-  trap_data()$events$peak_nm_index
    
      golem::print_dev('overlay')
      # if(nrow(excluded_events) == 0 ){
      # overlay_dy <-  dygraphs::dygraph(d) %>% #raw_data_dygraph
      #                   dygraphs::dySeries('raw', color = 'black', strokeWidth = 2) %>%
      #                   dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) %>%
      #                   dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') %>%
      #                   add_shades(periods_df) %>% #raw_periods
      #                   #add_shades(excluded_events, color = "grey60") %>%
      #                   add_labels_hmm(trap_data()$events, peak_nm_index = pni, labelLoc = 'bottom') %>% #results$events
      #                   dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
      #                   dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) %>%
      #                   dygraphs::dyUnzoom()
      # } else {
        
        overlay_dy <-  dygraphs::dygraph(d) %>% #raw_data_dygraph
                        dygraphs::dySeries('raw', color = 'black', strokeWidth = 2) %>%
                        dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) %>%
                        dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') %>%
                        add_shades(periods_df) %>% #raw_periods
                        #add_shades(excluded_events, color = "#BDBDBD") %>%
                        add_labels_hmm(trap_data()$events, peak_nm_index = pni, labelLoc = 'bottom') %>% #results$events
                        dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
                        dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) %>%
                        dygraphs::dyUnzoom()
     # }
        
  })
  
  output$mv_by_state <- renderPlot({
    
   req(trap_data())
    mv_data <- trap_data()$running
    mv_data$state <- factor(mv_data$state, levels = c(1, 2))
    
    mv1 <- ggplot2::ggplot(mv_data)+
              geom_point(aes(x = run_mean, y = run_var, color = state), size = 3, alpha = 0.5)+
              scale_color_manual(values = c("#1B9E77", "#D95F02"))+
              ggtitle('Mean-Variance (overlayed)')+
              ylab('Variance')+
              xlab('Mean (nm)')+
              theme_linedraw(base_size = 18)+
              theme(legend.position = 'none')

    mv2 <- ggplot(mv_data)+
              geom_point(aes(x = run_mean, y = run_var, color = state), size = 3, alpha = 0.5)+
              scale_color_manual(values = c("#1B9E77", "#D95F02"))+
              facet_wrap(~state)+
              ggtitle('Mean-Variance (by state)')+
              ylab('')+
              xlab('Mean (nm)')+
              theme_linedraw(base_size = 18)

      gridExtra::grid.arrange(mv1, mv2, nrow = 1)
  })
  
  output$observation <- renderValueBox({
    req(trap_data())
    valueBox(
      unique(trap_data()$trap$obs),
      paste(f$conditions$name, f$date_input),
      icon = icon("folder-open"),
      color = 'yellow'
    )
    
  })
  output$n_events <- renderValueBox({
    req(trap_data())
    valueBox(
      nrow(trap_data()$events),
      "Events",
      icon = icon("slack-hash"),
      color = 'yellow'
    )
    
  })
  
  output$s2n <- renderValueBox({
    req(trap_data())
    valueBox(
      round(unique(trap_data()$running$var_signal_ratio), 2),
      "Signal-to-noise",
      icon = icon("signal"),
      color = 'yellow'
    )
    
  })


  output$selected_folders <- renderPrint({
    ob <- if(input$which_obs == 'single'){
      validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select an obs folder to analyze'))
      f$obs$name 
    } else {
      validate(need(f$date_input, message = 'Please select date folder to analyze'))
      'Analyze all'
    }
    cat('Project:', f$project$name, ' | Conditions:', f$conditions$name, ' | Date:', f$date$name, ' | Observation:', ob)
  })

  
  info <- eventReactive(input$info_table, {
    defend_if_empty(f$date, ui = 'Please select a date folder', type = 'error')
    showNotification('Refreshing table', type = 'message')
    files <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
    trap_data <- purrr::map(files$path, vroom::vroom, delim = ",")
    # if('hm_overlay' %not_in% colnames(trap_data)){
    #  t <- trap_data  %>% 
    #   tidyr::nest(data = c(raw_bead, processed_bead)) 
    # } else {
    #   t <- trap_data  %>% 
    #     tidyr::nest(data = c(raw_bead, processed_bead, hm_overlay)) 
    # }
    
    purrr::map_dfr(trap_data, function(trap_data){
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
      
      vroom::vroom_write(trap_reviewed, path = file.path(f$obs$path, 'trap-data.csv'), delim = ',')
      setProgress(1, detail = 'Done')
    })
     showNotification('Review saved' , type = 'message')
     shinyjs::click('info_table')
  })
  
  
  observe({
  if(input$advanced){
    shinyjs::show('w_width')
    shinyjs::show('hz')
    shinyjs::show('em_random_start')
  } else {
    shinyjs::hide('w_width')
    shinyjs::hide('hz')
    shinyjs::hide('em_random_start')
  }
  })
  
  # output$obs_2_review <-  renderPrint({
  #   
  #   validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select an obs folder to analyze'))
  #   
  #   validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select an obs folder to analyze'))
  #   cat('Revewing ', f$obs$name)
  #   
  # })
  
    
}
    
## To be copied in the UI
# mod_hm_model_ui("hm_model_ui")
    
## To be copied in the server
# callModule(mod_hm_model_server, "hm_model_ui")
 
