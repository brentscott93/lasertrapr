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
      column(3, 
       
             box(width = NULL, title = 'HM-Model Analyzer Controls',
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
                 conditionalPanel(conditions = 'input.advanced == true', ns = ns, 
                                 shinyWidgets::switchInput( inputId = 'em_random_start',
                                                            label = "EM Rand. Start", 
                                                            width = '100%')
                                                          
                 ),
                
                 actionButton(inputId = ns("analyze_trap"),
                              label = "Run Analysis",
                              icon = icon("running"),
                              width = "100%",
                              style = ' margin-top: 25px;')
             
    ), 
    
    
    box(width = NULL, title = 'Review Analysis',
        actionButton(ns('view_results'), 
                     'View Results',
                     width = '100%',
                     icon = icon('line-graph'),
                     style = 'margin-bottom: 25px;'),
        shinyWidgets::radioGroupButtons(
          inputId = ns('quality_control'),
          label = "Should analysis be accepted?",
          choices = c("No" = FALSE,
                      "Yes" = TRUE),
          justified = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: black"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: black"))
        ),
        actionButton(ns('save_review'), 
                     'Save Review', 
                     width = '100%',
                     style = "color: #fff; background-color: #b80f07; margin-top: 25px;")
        
    )
      ), #col close
  
    column(9,
    box(width = NULL, title = 'Status Table',
        actionButton(ns('status_table'), 'Refresh status table'),
        gt::gt_output(ns('table'))
    ),
    ) # col close
    ),
    
    fluidRow(
      box(width = 12, title = 'Results', 
      background = 'black',  
      

          fluidRow(
      column(2,
             valueBoxOutput(ns('n_events'), width = NULL),
             valueBoxOutput(ns('s2n'), width = NULL)
      ),
      column(10, 
             box(width = NULL, title = 'MV Plot by State',
                 plotOutput(ns('mv_by_state')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
             )
      )),
      fluidRow(
             box(width = 4, title = 'HM-Model Summary',
                 verbatimTextOutput(ns('hm_model_summary')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
             ),
      box(width = 8, title = 'Event Frequency',
          plotOutput(ns('event_freq')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
      )
      ), #rowclose
    
    fluidRow(
      box(width = 12, title = 'HM-Model Data (Running Mean & Variance)',
          dygraphs::dygraphOutput(ns('hm_model_dygraph1'), height = "250px") %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"),
          dygraphs::dygraphOutput(ns('hm_model_dygraph2'), height = "250px") %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
      ),
    
    ),
    # fluidRow(
    #   box(width = 12, title = 'Running Mean (HM-Model Data)',
    #      
    #   ),
      
    #),
    fluidRow(
      box(width = 12, title = 'Raw Data + HM-Model Overlay',
             dygraphs::dygraphOutput(ns('overlay_dygraph')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
    )
    ) #rowclose
      ) #results box close
  )
  )#taglist clost
}
    
#' hm_model Server Function
#' @importFrom magrittr "%<>%"
#' @noRd 
mod_hm_model_server <- function(input, output, session, f){
  ns <- session$ns
 
  observeEvent(input$analyze_trap, {
    
    trap_data_rds <- get_status_table(f$date, f$date_input)
    rds_file_path_data <- list_files(f$date$path, pattern = 'trap-data.rds', recursive = T)
    trap_data_rds %<>% mutate(rds_file_path = rds_file_path_data$path)
    
    if(input$which_obs =='single'){
      if(is_empty(f$obs_input)) showNotification('Select an obs', type = 'error')
      req(!is_empty(f$obs_input))
      trap_data_rds <- get_status_table(f$date, f$date_input) %>% 
        dplyr::filter(obs == f$obs_input)
    }
    
    if(is.null(input$em_random_start)){
      em_start <- FALSE
    } else {
      em_start <- input$em_random_start
    }
      trap_data_rds %<>% dplyr::filter(include == T)
      hidden_markov_analysis(trap_data_rds, f, em_random_start = em_start)
      
      shinyWidgets::sendSweetAlert(session = session,
                                   title =  "Hidden Markov Analysis Complete",
                                   text = "Results saved to 'lasertrapr' folder",
                                   type = "success")
    
  })
  
  hm <- reactiveValues(go = 0)
  observeEvent(input$view_results, {
   if(is_empty(f$obs_input)) showNotification('Select an obs', type = 'error')
   req(!is_empty(f$obs_input))
   if(substring(f$obs_input, 1, 3) != 'obs') showNotification('Select an obs', type = 'error')
   req(substring(f$obs_input, 1, 3) == 'obs')
   showNotification('Loading results...This may take a second', type = 'warning')
   hm$go <- hm$go + 1
  })
   observeEvent(hm$go, ignoreInit = T, {
     req(!is_empty(f$obs_input))
     
     p <- list_files(f$obs$path, pattern =  'trap-data.rds')
     t <- readRDS(p$path)
    hm$results <- t$results[[1]]
     
  })
   output$hm_model_dygraph1 <- dygraphs::renderDygraph({
     req(hm$results)
     showNotification('Loading run var graph', type = 'message')
     dy_rv <- hm$results$dygraph_master_list$dy_rv
     c <- hm$results$dygraph_master_list$c
     shades_df <- hm$results$dygraph_master_list$shades_df
     shade_col <- '#E2E2E2'
     
     dygraphs::dygraph(dy_rv, group = 'group') %>%
       dygraphs::dySeries('rv', color = c[[1]], strokeWidth = 2) %>%
       dygraphs::dyAxis('x', axisLineColor = '#FFFFFF', drawGrid = FALSE, axisLabelColor = '#FFFFFF') %>%
       dygraphs::dyAxis('y', label = 'Running Variance', drawGrid = FALSE,) %>%
       add_shades(periods = shades_df, color = shade_col) %>%
       dygraphs::dyUnzoom()
   })
  output$hm_model_dygraph2 <- dygraphs::renderDygraph({
    req(hm$results)
    showNotification('Loading run mean graph', type = 'message')
    dy_rm <- hm$results$dygraph_master_list$dy_rm
    c <- hm$results$dygraph_master_list$c
    shades_df <- hm$results$dygraph_master_list$shades_df
    shade_col <- '#E2E2E2'
    
    dygraphs::dygraph(dy_rm, group = 'group') %>%
      dygraphs::dySeries('rm', color = c[[2]],  strokeWidth = 2) %>%
      dygraphs::dyAxis('x', label = 'Window', drawGrid = FALSE) %>%
      dygraphs::dyAxis('y', label = 'Running Mean (nm)', drawGrid = FALSE) %>%
      add_shades(periods = shades_df, color = shade_col) %>%
      dygraphs::dyRangeSelector(fillColor =c[[3]], strokeColor = c[[8]])
    
  })
  
  
  output$overlay_dygraph <- dygraphs::renderDygraph({
    req(hm$results)
    showNotification('Loading raw data overlay', type= 'message')
    d <- hm$results$dygraph_master_list$raw_data_dygraph
    events <- hm$results$events
    periods_df <- hm$results$dygraph_master_list$raw_periods
    pni <- hm$results$dygraph_master_list$peak_nm_index
    dygraphs::dygraph(d) %>% #raw_data_dygraph
      dygraphs::dySeries('raw', color = '#242424', strokeWidth = 2) %>%
      dygraphs::dySeries('model', color = '#ff41c8',  strokeWidth = 2) %>%
      dygraphs::dyRangeSelector() %>%
      add_shades(periods_df, color = '#ffd2df') %>% #raw_periods
      add_labels_hmm(events, peak_nm_index = pni, labelLoc = 'bottom') %>% #results$events
      dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
      dygraphs::dyAxis('y', label = 'nm') %>%
      dygraphs::dyUnzoom()
    
  })
  
  output$event_freq <- renderPlot({
    req(hm$results)
    showNotification('Loading event frequency', type = 'message')
    hm$results$event_freq$plot
  })
  output$n_events <- renderValueBox({
    req(hm$results)
    valueBox(
      nrow(hm$results$events),
      "Events",
      icon = icon("slack-hash"),
      color = 'fuchsia'
    )
    
  })
  
  output$s2n <- renderValueBox({
    req(hm$results)
    valueBox(
      round(hm$results$s2n, 2),
      "Signal-to-noise",
      icon = icon("signal"),
      color = 'purple'
    )
    
  })
  
  output$hm_model_summary <- renderPrint({
    req(hm$results)
    showNotification('Loading model summary', type = 'message')
    summary(hm$results$hmm_fit)
  })
  
  output$mv_by_state <- renderPlot({
    req(hm$results)
    showNotification('Loading MV Plots', type = 'message')
    plot(hm$results$mv_state_plot)
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
  
  
 
  status <- reactiveValues()
  
  observeEvent(input$status_table, {
   status$df <-  get_status_table(f$date, f$date_input) %>% 
      dplyr::select(project, conditions, date, obs, processed_how, mv2nm, nm2pn, include, status, analyzer, report, quality_control)
   showNotification('Status table refreshed', type = 'message')
  })
  
  output$table <- gt::render_gt(width = gt::pct(100),{
    req(status$df)
    status$df %>% 
      rename('Project' = project, 
             'Conditions' = conditions,
             'Date' = date, 
             'Obs' = obs,
             'Processor' = processed_how,
             'mV-to-nm' = mv2nm,
             'nm-to-pN' = nm2pn,
             'Include' = include, 
             'Status' = status,
             'Analyzer' = analyzer,
             'Report' = report,
             'Review' = quality_control) %>% 
      gt::gt(rowname_col = 'Obs') %>% 
      gt::tab_stubhead('Obs') %>% 
      gt::tab_header(
        title = "Current Status of Observations",
        subtitle = "Green indicates ready"
      )  %>%
      gt:: tab_spanner(
        label = "Description",
        columns = vars(Project, Conditions, Date)
      ) %>% 
      gt::tab_spanner(
        label = "Progress Indicators",
        columns = vars(Processor, `mV-to-nm`, `nm-to-pN`, Include, Status, Analyzer, Report, Review)
      ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "red"),
        locations = gt::cells_body(
          rows = Status %in% c('grouped', 'processed'))
      ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "green"),
        locations = gt::cells_body(
          rows = Report == 'success')) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "orange"),
        locations = gt::cells_body(
          rows = Report != 'success' & Status == 'analyzed'))
      
    
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
 
