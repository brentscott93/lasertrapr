#' hm_model UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList 
#' @noRd 
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
       
      fluidRow(
        column(6, 
                actionButton(inputId = ns("options"),
                             label = "Options",
                             icon = icon("cog"),
                             width = "100%",
                             style = 'margin-top: 25px;')
        ),
        column(6,
                actionButton(inputId = ns("analyze_trap"),
                             label = "Run Analysis",
                             icon = icon("running"),
                             width = "100%",
                             style = 'margin-top: 25px;')
        )
      )
    )
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
             tabBox(width = NULL, title = 'Insights', side = 'right',
               tabPanel("MV Plot",
                 plotOutput(ns('mv_by_state')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
               ),
               tabPanel("Numbers",
                        plotOutput(ns("numbers"))
                        )
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
        actionButton(ns("review_options"), "Review Options"),
        actionButton(ns("snapshot"), "", icon = icon("camera")),
        dygraphs::dygraphOutput(ns('overlay_dygraph'), height = '400px') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
    )
      )
    # ) #rowclose
    #   ) #results box close
    )
  )#taglist clost
}
    
#' hm_model Server Function
#' @noRd
#' @param input,output,session,f module parameters
mod_hm_model_server <- function(input, output, session, f){
 ns <- session$ns


 observeEvent(f$obs$path, {
   req(f$obs$path)
    o <- fread(file.path(f$obs$path, "options.csv"))
   if(is.null(o$channels)){
     o$channels <- 1
   }
   if(o$channels == 1){
    hm$use_channel_options <- c("Mean/Var", "Variance")
   } else {
    hm$use_channel_options <- c("Covariance", "Mean/Var", "Variance")
    }
 })

 hm <- reactiveValues(analyze = 0, use_channels_options = c("Mean/Var", "Variance"))
 
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
      trap_data <- purrr::map(file$path, data.table::fread, nrows = 1)
    } else {
      files <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
      trap_data <- purrr::map(files$path, data.table::fread, nrows = 1)
    }
    opt <- reactiveValuesToList(isolate(a))
    withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
    purrr::walk(trap_data, ~hidden_markov_changepoint_analysis(
                                    trap_data = .x,
                                    f = f,
                                    hz = a$hz,
                                    w_width = a$w_width,
                                    w_slide = a$w_slide,
                                    use_channels = a$use_channels,
                                    em_random_start = a$em_random_start, 
                                    front_cp_method = a$front_cp_method,
                                    back_cp_method = a$back_cp_method, 
                                    cp_running_var_window = a$cp_running_var_window,
                                    displacement_type = a$displacement_type,
                                    opt = opt,
                                    is_shiny = TRUE)
              )
       })
      
      shinyWidgets::sendSweetAlert(session = session,
                                   title =  "Analysis Complete",
                                   text = "Results saved to 'lasertrapr' folder",
                                   type = "success")
     shinyjs::click('info_table')
  })
  
  
  trap_data <- eventReactive(input$view_results, {
    defend_if_empty(f$obs, ui = 'Please select an obs folder', type = 'error')
    defend_if_blank(f$obs_input, ui = 'Please select an obs folder', type = 'error')
  
    filenames <- c('trap-data.csv', 'measured-events.csv', 'hm-model-data.csv', 'options.csv')
    paths <- purrr::map(filenames, ~list_files(f$obs$path, pattern = .x))
    data <-  purrr::map(paths, ~data.table::fread(.x$path))
    names(data) <- c('trap', 'events', 'running', 'options')
    data
  })
  

  output$overlay_dygraph <- dygraphs::renderDygraph({
    req(trap_data())
    hz <- trap_data()$options$hz[[1]]
    channels <- trap_data()$options$channels[[1]]
    ## if(is.null(channels)) channels <- 1
    ## if(channels == 1){
      raw <- trap_data()$trap$processed_bead
    ## } else if(channels == 2){
      ## if(trap_data()$options$preferred_channel[[1]] == 1){
        ## raw <- trap_data()$trap$processed_bead_1
      ## } else {
        ## raw <- trap_data()$trap$processed_bead_2
      ## }
    ## }
    d <- data.frame(index = (1:nrow(trap_data()$trap)/hz),
                    raw = raw,
                    model = trap_data()$trap$hm_overlay)

    periods_df <- data.table::data.table(start = trap_data()$events$cp_event_start_dp/hz,
                             stop = trap_data()$events$cp_event_stop_dp/hz,
                             keep = trap_data()$events$keep,
                             event_user_excluded = trap_data()$events$event_user_excluded, 
                             front_signal_ratio = trap_data()$events$front_signal_ratio,
                             back_signal_ratio = trap_data()$events$back_signal_ratio,
                             color = scales::alpha("#D95F02" , 0.4))


   periods_df <- periods_df[keep == T & event_user_excluded == F]
   periods_df <- periods_df[front_signal_ratio >= ro$front_signal_ratio & back_signal_ratio >= ro$back_signal_ratio]


    # add a column providiing the real event number
    # so when user filters out events, the events retain their real event number
    # making it easier to pick other events to exclude
   events <- trap_data()$events
   events$id <- 1:nrow(trap_data()$events)
   labels <- events[keep == T & event_user_excluded == F]

    # get the peak nm index to put the labels here
   ## pni <-  events$peak_nm_index
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
        
        overlay_dy <-  dygraphs::dygraph(d) |> #raw_data_dygraph
                        dygraphs::dySeries('raw', color = 'black') |>
                        dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) |>
                        dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') |>
                        add_shades(periods_df) |> #raw_periods
                        #add_shades(excluded_events, color = "#BDBDBD") %>%
                        add_labels_hmm(labels, labelLoc = 'bottom') |> #results$events
                        dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) |>
                        dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) |>
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
      nrow(trap_data()$events[keep == T]),
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
    files <- list_files(f$date$path, pattern = 'options.csv', recursive = T)
    purrr::map_df(files$path, ~data.table::fread(.,
                                          select = c("obs", "include", "analyzer", "report", "review"),
                                           nrows = 1))
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
      
      td <- list_files(f$obs$path, pattern = 'options.csv')
      trap <- data.table::fread(td$path)
      setProgress(0.7)
      trap_reviewed <- trap %>% 
        dplyr::mutate(review = input$quality_control)
      
      data.table::fwrite(trap_reviewed, file = file.path(f$obs$path, 'options.csv'))
      setProgress(1, detail = 'Done')
    })
     showNotification('Review saved' , type = 'message')
     shinyjs::click('info_table')
  })
  
  
  
  
  # output$obs_2_review <-  renderPrint({
  #   
  #   validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select an obs folder to analyze'))
  #   
  #   validate(need(substring(f$obs_input, 1, 3) == 'obs', message = 'Please select an obs folder to analyze'))
  #   cat('Revewing ', f$obs$name)
  #   
  # })
  
    a <- reactiveValues(w_width = 150,
                        w_slide = "1/2",
                        em_random_start = FALSE, 
                        use_channels = "Mean/Var",
                        front_cp_method = "Mean/Var",
                        back_cp_method = "Mean/Var",
                        cp_running_var_window = 5,
                        displacement_type = "avg")
    
    observeEvent(input$set_options, {
      a$w_width <- input$w_width
      a$w_slide <- input$w_slide
      a$em_random_start <- input$em_random_start
      a$use_channels <- input$use_channels
      a$front_cp_method <-input$front_cp_method
      a$back_cp_method <-input$back_cp_method
      a$cp_running_var_window <- input$cp_running_var_window
      a$displacement_type <- input$displacement_type
      removeModal()
    })
    
    #### Analysis Options ####
  observeEvent(input$options, {
    showModal(
      modalDialog(
        title = "Set Analysis Parameters",
        footer = tagList(modalButton("Cancel"), actionButton(ns("set_options"), "OK")),
        tabsetPanel(
          tabPanel("HM-Model",
                       fluidRow(
                         column(6, 
                                numericInput(ns("w_width"), "Window Width", value = a$w_width, width = "100%")
                         ), 
                         column(6, 
                                shinyWidgets::sliderTextInput(ns("w_slide"), 
                                                              "Slide Window", c("1-Pt", "1/4", "1/2", "3/4", "No-overlap"),
                                                              grid = TRUE, 
                                                              selected = a$w_slide, 
                                                              width = "100%")
                         )
                       ) ,
                       fluidRow(
                         column(6, 
                                shinyWidgets::prettyRadioButtons(
                                  inputId = ns("use_channels"),
                                  label = "Channels", 
                                  choices = hm$use_channels_options,
                                  selected = a$use_channels,
                                  inline = TRUE,  
                                  status = "primary",
                                  fill = TRUE
                                )
                            ),
                         column(4,
                                div(style = "margin-top: 25px;",
                                shinyWidgets::prettyCheckbox(inputId = ns('em_random_start'),
                                                             value = a$em_random_start,
                                                             label = "EM Random Start?", 
                                                             status = "primary",
                                                             shape = "curve",
                                                             outline = TRUE)
                          )
                         )
                        )
        ), #hm-model tab close
        tabPanel("Changepoint", 
                 fluidRow(
                   column(6, 
                          h4("Front"),
                          shinyWidgets::prettyRadioButtons(
                            inputId = ns("front_cp_method"),
                            label = "Channels", 
                            choices = c("Mean/Var", "Variance"),
                            selected = a$front_cp_method,
                            inline = TRUE,  
                            status = "primary",
                            fill = TRUE
                          )
                   ),
                          column(6, 
                                 h4("Back"),
                                 shinyWidgets::prettyRadioButtons(
                                   inputId = ns("back_cp_method"),
                                   label = "Channels", 
                                   choices = c("Mean/Var", "Variance"),
                                   selected = a$back_cp_method,
                                   inline = TRUE,  
                                   status = "primary",
                                   fill = TRUE
                                 )
                          )
                   ),
                    sliderInput(ns("cp_running_var_window"), "Running Variance Window Width", min = 5, max = 100, value = a$cp_running_var_window, width = "100%")
          ), #cp tab panel
        tabPanel("Displacements",
                 radioButtons(inputId = ns('displacement_type'),
                              label = 'Displacement Calculation Method', 
                              choices = list("Average" = "avg",
                                             "Peak" = "peak"),
                              inline = TRUE,
                              selected = a$displacement_type)
         )
        )
       )
      )
    
    observe({
      if(is.null(input$back_cp_method)){
        shinyjs::hide("cp_running_var_window")
      } else {
        if(input$back_cp_method == "Variance" || input$front_cp_method == "Variance"){
          shinyjs::show("cp_running_var_window")
        } else  {
          shinyjs::hide("cp_running_var_window")
        }
      }
      })
  })
    
    #### Review Options ####
    ro <- reactiveValues(front_signal_ratio = 0,
                         back_signal_ratio = 0)
    
    which_events_user_excluded <- eventReactive(trap_data()$events, {
      req(trap_data())
      which(trap_data()$events$event_user_excluded == TRUE)
    })
    output$number_excluded_events <- renderText({
      paste0("Events excluded: ", toString(which_events_user_excluded()))
    })
    observeEvent(input$review_options, {
      showModal(
        modalDialog(
          title = "Review Options",
          footer = tagList(modalButton("Cancel"), actionButton(ns("set_review_options"), "OK")),
          size = "l",
          tabsetPanel(
            tabPanel("Exclude Events",
            br(), 
            column(3,    
              selectInput(ns("exclude_event_manual"), 
                          label = "Select events to modify",
                          choices = 1:nrow(trap_data()$events),
                          multiple = T
                          )
                        ),
            column(3,
              shinyWidgets::radioGroupButtons(
                inputId = ns('include_exclude'),
                label = "Include or Exclude?",
                choices = c("Include" = "include",
                            "Exclude" = "exclude"),
                selected = "exclude",
                justified = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-check-square",
                               style = "color: black"),
                  no = tags$i(class = "fa fa-square-o",
                              style = "color: black"))
                        )
              ),
              div(style = "padding-top: 15px", verbatimTextOutput(ns("number_excluded_events")))
                    
            ),
            tabPanel("Signal Filter",
                     column(6, 
                            sliderInput(ns("front_signal_ratio"),
                                        "Front Signal-to-noise Filter",
                                        min = 0, 
                                        max = 4,
                                        value = 0, 
                                        step = 0.1)
                            ),
                            column(6, 
                                   sliderInput(ns("back_signal_ratio"),
                                               "Back Signal-to-noise Filter",
                                               min = 0, 
                                               max = 4,
                                               value = 0, 
                                               step = 0.1)
                            )
            )
          )
        )
      )
    })
    
    observeEvent(input$snapshot, {
      showModal(
        modalDialog(
          title = "Snapshot",
          footer = tagList(modalButton("Cancel"), actionButton(ns("save_plot_overlay"), "Save")),
          size = "l",
             fluidRow(
               column(2,
                      colourpicker::colourInput(ns("export_plot_event_color"),
                                                label = "Events Color",
                                                value = "red",
                                                showColour = "both"),
               ),
               column(4, 
                      sliderInput(ns("export_plot_height"),
                                  label = "Height (in)",
                                  min = 0,
                                  max = 20,
                                  step = 0.5,
                                  value = 2.5),
               ), 
               column(4, 
                      sliderInput(ns("export_plot_width"),
                                  label = "Width (in)",
                                  min = 0,
                                  max = 50,
                                  step = 1,
                                  value = 18),
               ),
               column(2, 
                      div(style = "padding-top: 30px;",
                        checkboxInput(ns("save_as_gg"), 
                                      "Save as ggplot?", 
                                      FALSE)
                      )
               
               )
             ),
          fluidRow(column(2, checkboxInput(ns("add_stage"), "Add Stage", value = FALSE))),
             fluidRow(
               plotOutput(ns("export_ggplot"), height = "175px")
             ),
             fluidRow(
               plotOutput(ns("gg_stage"), height = "175px")
             )
        )
      )
    })
    
    observeEvent(input$set_review_options, {
      ro$front_signal_ratio <- input$front_signal_ratio
      ro$back_signal_ratio <- input$back_signal_ratio
      golem::print_dev("before if")
      if(!rlang::is_empty(input$exclude_event_manual)){
       excluded_data <- trap_data()$events
       golem::print_dev("setting val true")
       if(input$include_exclude == "exclude"){
       excluded_data$event_user_excluded[as.numeric(input$exclude_event_manual)] <- TRUE
       } else if(input$include_exclude == "include"){
       excluded_data$event_user_excluded[as.numeric(input$exclude_event_manual)] <- FALSE
       }
       data.table::fwrite(excluded_data, file = file.path(f$obs$path, 'measured-events.csv'))
       showNotification(paste0("The following events were modified: ", toString(input$exclude_event_manual), " - Refreshing..."), duration = 3, type = "message")
      }
      removeModal()
      shinyjs::click("view_results")
    })
 
    observe({
    output$export_ggplot <- renderPlot(height = 175, {
      plot_overlay(obs_path = f$obs$path,
                   time_period_dp = input$overlay_dygraph_date_window,
                   color = input$export_plot_event_color)
    })
    
    })
    
    observeEvent(input$save_plot_overlay,{
      dir_summary <- file.path(f$project$path, "summary")
      if(!dir.exists(dir_summary)) dir.create(dir_summary)
      
      dir_fig <- file.path(dir_summary, "figures")
      if(!dir.exists(dir_fig)) dir.create(dir_fig)
      
      file_name <- file.path(dir_fig, 
                             paste0(
                              f$conditions$name,
                              "_",
                              f$date$name,
                              "_",
                              f$obs$name,
                              "_",
                              round(input$overlay_dygraph_date_window[[1]], 4),
                              "-",
                              round(input$overlay_dygraph_date_window[[2]], 4)
                              )
                             )
      
      gg <- plot_overlay(obs_path = f$obs$path,
                         time_period_dp = input$overlay_dygraph_date_window,
                         color = input$export_plot_event_color)

      if(!input$add_stage){
      if(input$save_as_gg){
        saveRDS(gg, file = paste0(file_name, ".rds"))
      } else {
        cowplot::ggsave2(file = paste0(file_name, ".pdf"),
                         plot = gg,
                         height =  as.numeric(input$export_plot_height),
                         width = as.numeric(input$export_plot_width),
                         units = "in")
      }

      } else {
        ## add stage and save

        opt_dat <- data.table::fread(file.path(f$obs$path, "options.csv"))
        time_period_dp <- c(round_any(input$overlay_dygraph_date_window[[1]], 1/opt_dat$hz, f = round),
                            round_any(input$overlay_dygraph_date_window[[2]], 1/opt_dat$hz, f = round))
        time_period_dp <- time_period_dp*opt_dat$hz
        stage_dat <- data.table::fread(file.path(f$obs$path, "trap-data.csv"))
        stage_dat <- stage_dat[time_period_dp[[1]]:time_period_dp[[2]]]
        stage_dat$time <- seq(1/opt_dat$hz, nrow(stage_dat)/opt_dat$hz, by = 1/opt_dat$hz)
        gg_stage <- ggplot(stage_dat)+geom_line(aes(time, stage_position))+theme_void()
        gg2 <-  cowplot::plot_grid(gg, gg_stage, nrow = 2)
      if(input$save_as_gg){
        saveRDS(gg, file = paste0(file_name, ".rds"))
        saveRDS(gg_stage, file = paste0(file_name, "_STAGE", ".rds"))
      } else {
        cowplot::ggsave2(file = paste0(file_name, ".pdf"),
                         plot = gg2,
                         height =  as.numeric(input$export_plot_height)*2,
                         width = as.numeric(input$export_plot_width),
                         units = "in")
      }

      }
      showNotification("Plot Saved", type = "message")
      removeModal()
    })
    
    output$numbers <- renderPlot({
      
      req(trap_data())
      measured_events <- trap_data()$events
      step <- 
        ggplot()+
        ## geom_dotplot(data = measured_events, aes(displacement_nm),
        ##              fill = "grey40",
        ##              binwidth = 2)+
        stat_ecdf(data = measured_events, aes(x = displacement_nm), pad = FALSE)+
        geom_vline(aes(xintercept = mean(measured_events$displacement_nm)), linetype = "dashed")+
        geom_label(aes(mean(measured_events$displacement_nm), 
                       y = 1, 
                       label = paste0("bar(x)==", round(mean(measured_events$displacement_nm), 1))),
                   parse = TRUE,
                   size = 6)+
        ggtitle("Displacements")+
        xlab("nanometers")+
        ylab('ECDF')+
        # scale_x_continuous(breaks = sort(c(seq(-100, 100, by = 20), round(mean(measured_events$displacement_nm), 1))))+
        cowplot::theme_cowplot(18)+
        theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      
      time_on <- 
        ggplot()+
        stat_ecdf(data = measured_events, aes(x = time_on_ms),
                     pad = FALSE)+
        geom_vline(aes(xintercept = mean(measured_events$time_on_ms)), linetype = "dashed")+
        geom_label(aes(mean(measured_events$time_on_ms), y = 1, 
                       label = paste0("bar(x)==", round(mean((measured_events$time_on_ms), 0)))),
                   parse = TRUE,
                   size = 6)+
        ggtitle("Time On")+
        xlab("milliseconds")+
        ylab('ECDF')+
        cowplot::theme_cowplot(18)+
        theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      
      cowplot::plot_grid(step, time_on)
    })
    
}
    
## To be copied in the UI
# mod_hm_model_ui("hm_model_ui")
    
## To be copied in the server
# callModule(mod_hm_model_server, "hm_model_ui")
 
