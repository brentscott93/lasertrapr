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
                              style = ' margin-top: 25px; color: #fff; background-color: #F012BE;')
             
    ), 
    
    
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
                     style = "color: #fff; background-color: #605ca8; margin-top: 25px;")
        
    )
      ), #col close
  
    column(9,
    box(width = NULL, title = 'Status Table',
        actionButton(ns('status_table'), 'Refresh status table'),
        gt::gt_output(ns('table')) %>%  shinycssloaders::withSpinner(type = 8, color = "#373B38")
    ),
    ) # col close
    ),
    
    fluidRow(
      box(width = 12, title = 'Results', 
      background = 'black',  
      

          fluidRow(
      column(2,
             valueBoxOutput(ns('observation'), width = NULL),
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
      box(width = 8, title = 'Real Time Event Frequency',
          plotOutput(ns('event_freq')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
      )
      ), #rowclose
    
    fluidRow(
      box(width = 12, title = 'HM-Model Data (Running Mean & Variance)', collapsible = T, collapsed = T, 
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
      box(width = 12, title = 'Raw Data + HM-Model Overlay', collapsible = T, collapsed = T, 
             dygraphs::dygraphOutput(ns('overlay_dygraph'), height = '400px') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
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
    golem::print_dev(input$which_obs)
    if(is_empty(f$date_input)) showNotification('Select a date', type = 'error')
    req(!is_empty(f$date_input))
    if(input$which_obs == 'single'){
      if(is_empty(f$obs_input)) showNotification('Select an obs', type = 'error')
      req(!is_empty(f$obs_input))
      if(substring(f$obs_input, 1, 3) != 'obs') showNotification('Select an obs', type = 'error')
      req(substring(f$obs_input, 1, 3) == 'obs')
    }
      
      showNotification('Analysis will begin shortly...', type = 'message')
      
    
      hm$analyze <-   hm$analyze + 1
      
  })
  
  observeEvent(hm$analyze, ignoreInit = T, {
    trap_data_rds <- get_status_table(f$date, f$date_input)
    
    if(input$which_obs =='single'){
    trap_data_rds %<>% dplyr::filter(obs == f$obs_input)
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
     shinyjs::click('status_table')
  })
  
  hm <- reactiveValues(go = 0, analyze = 0)
  
  observeEvent(input$view_results, {
    if(is_empty(f$obs_input)) showNotification('Select an obs', type = 'error')
    req(!is_empty(f$obs_input))
    if(substring(f$obs_input, 1, 3) != 'obs') showNotification('Select an obs', type = 'error')
    req(substring(f$obs_input, 1, 3) == 'obs')
    showNotification('Hang tight, this will take a few seconds.', type = 'message')
    hm$go <- hm$go + 1
  })

   observeEvent(hm$go, ignoreInit = T, {
     req(!is_empty(f$obs_input))
     withProgress(message = 'Loading', detail = 'This may take a while...', {
       setProgress(0.3)
     p <- list_files(f$obs$path, pattern =  'viz.rds')
     setProgress(0.7)
     viz <- readRDS(p$path)
     setProgress(0.9)
      hm$results <- viz$plot[[1]]
      setProgress(1)
     })
  })
   output$hm_model_dygraph1 <- dygraphs::renderDygraph({
     req(hm$results)
     showNotification('Loading run var graph', type = 'message')
     hm$results$rv_dy
   })
  output$hm_model_dygraph2 <- dygraphs::renderDygraph({
    req(hm$results)
    showNotification('Loading run mean graph', type = 'message')
    hm$results$rm_dy
  })
  
  
  output$overlay_dygraph <- dygraphs::renderDygraph({
    req(hm$results)
    showNotification('Loading raw data overlay', type= 'message')
    hm$results$overlay_dy
    
  })
  
  output$event_freq <- renderPlot({
    req(hm$results)
    showNotification('Loading event frequency', type = 'message')
    plot(hm$results$event_freq_plot)
  })
  
  output$observation <- renderValueBox({
    req(hm$results)
    valueBox(
      f$obs_input,
      paste(f$conditions$name, f$date_input),
      icon = icon("folder-open"),
      color = 'fuchsia'
    )
    
  })
  output$n_events <- renderValueBox({
    req(hm$results)
    valueBox(
      nrow(hm$results$events),
      "Events",
      icon = icon("slack-hash"),
      color = 'purple'
    )
    
  })
  
  output$s2n <- renderValueBox({
    req(hm$results)
    valueBox(
      round(hm$results$s2n, 2),
      "Signal-to-noise",
      icon = icon("signal"),
      color = 'fuchsia'
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
    showNotification('Refreshing table', type = 'message')
   status$df <-  get_status_table(f$date, f$date_input) %>% 
      dplyr::select(project, conditions, date, obs, processed_how, mv2nm, nm2pn, include, status, analyzer, report, quality_control)
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
        style = gt::cell_fill(color = purple()),
        locations = gt::cells_body(
          rows = Status %in% c('grouped', 'processed'))
      ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = pink()),
        locations = gt::cells_body(
          rows = Status == 'analyzed' & !is.na(Review))
        ) %>% 
      gt::tab_style(
        style = gt::cell_fill(color = maroon()),
        locations = gt::cells_body(
          rows = Status == 'analyzed' & is.na(Review)))
      
    
  })
    
  
  
  
  #### SAVE ####
  
  observeEvent(input$save_review, {
    if(is_empty(f$obs_input)) showNotification('Select an obs', type = 'error')
    req(!is_empty(f$obs_input))
    if(substring(f$obs_input, 1, 3) != 'obs') showNotification('Select an obs', type = 'error')
    req(substring(f$obs_input, 1, 3) == 'obs')
  
    withProgress(message = 'Saving Review', {
      
      td <- list_files(f$obs$path, pattern = 'trap-data.rds')
      trap <- readRDS(td$path)
      setProgress(0.7)
      trap_reviewed <- trap %>% 
        dplyr::mutate(quality_control = input$quality_control)
      
      saveRDS(trap_reviewed, file = trap$rds_file_path)
      setProgress(1, detail = 'Done')
    })
     showNotification('Review saved' , type = 'message')
     shinyjs::click('status_table')
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
 
