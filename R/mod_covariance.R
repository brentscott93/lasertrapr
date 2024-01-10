#' covariance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_covariance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, title = "Selected Folders",
          verbatimTextOutput(ns('selected_folders'))
      )),
    fluidRow(
      column(4,

             box(width = NULL, title = 'Covariance Analyzer',
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
        DT::DTOutput(ns('table')) |>  shinycssloaders::withSpinner(type = 8, color = "#373B38")
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
               tabPanel("M-CV",
                 plotOutput(ns('hmm_state')) |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
               ),
               tabPanel("HMM/Covar",
                 plotOutput(ns('hmm_covar')) |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
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

#' covar hm_model Server Function
#' @noRd
#' @param input,output,session,f module parameters
mod_covariance_server <- function(input, output, session, f){

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
    purrr::walk(trap_data, ~covariance_hidden_markov_changepoint_analysis(
                                    trap_data = .x,
                                    f = f,
                                    hz = a$hz,
                                    w_width = a$w_width,
                                    w_slide = a$w_slide,
                                    ## median_w_width = a$median_w_width,
                                    em_random_start = a$em_random_start,
                                    front_cp_method = a$front_cp_method,
                                    back_cp_method = a$back_cp_method,
                                    ## cp_running_var_window = a$cp_running_var_window,
                                    opt = opt,
                                    merge_threshold_dp = a$merge_threshold_dp,
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
    d <- data.table(index = (1:nrow(trap_data()$trap)/hz),
                    bead_1 = trap_data()$trap$processed_bead_1,
                    bead_2 = trap_data()$trap$processed_bead_2
                    )

    if(nrow(d) >= 1000000 & nrow(d) <= 2000000){
     ds <- seq(1, nrow(d), by = 2)
     d <- d[ds]
    } else if(nrow(d) >= 2000000 & nrow(d) <= 3000000){
     ds <- seq(1, nrow(d), by = 3)
     d <- d[ds]
    } else if(nrow(d) >= 4000000){
     ds <- seq(1, nrow(d), by = 4)
     d <- d[ds]
    }

    me_data <- trap_data()$events
    periods_df <- data.table::data.table(
                                start = me_data$cp_event_start_dp_1/hz,
                                stop = me_data$cp_event_stop_dp_1/hz,
                                keep_1 = me_data$keep_1,
                                keep_2 = me_data$keep_2,
                                event_user_excluded = me_data$event_user_excluded,
                                color = scales::alpha("#FF0000" , 0.4)
                              )


   periods_df <- periods_df[keep_1 == T & keep_2 == T & event_user_excluded == F]

    periods_df2 <- data.table::data.table(
                                start = me_data$cp_event_start_dp_2/hz,
                                stop = me_data$cp_event_stop_dp_2/hz,
                                keep_1 = me_data$keep_1,
                                keep_2 = me_data$keep_2,
                                event_user_excluded = me_data$event_user_excluded,
                                color = scales::alpha("#0000FF" , 0.4)
                              )


   periods_df2 <- periods_df2[keep_1 == T & keep_2 == T & event_user_excluded == F]
    # add a column providiing the real event number
    # so when user filters out events, the events retain their real event number
    # making it easier to pick other events to exclude
   events <- trap_data()$events
   events$id <- 1:nrow(trap_data()$events)
   events$displacement_nm <- round((me_data$displacement_bead_1_nm + me_data$displacement_bead_2_nm)/2, 1)
   events$time_on_ms <- round((me_data$attachment_duration_bead_1_ms + me_data$attachment_duration_bead_2_ms)/2, 1)
    events$peak_nm_index <- round((events$displacement_marker_1 + events$displacement_marker_2) / 2, 0)/hz
   labels <- events[keep_1 == T & keep_2 == T & event_user_excluded == F]

        overlay_dy <-  dygraphs::dygraph(d) |> #raw_data_dygraph
                        dygraphs::dySeries('bead_1', color = 'black') |>
                        dygraphs::dySeries('bead_2', color = 'red') |>
                        dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') |>
                        add_shades(periods_df) |> #raw_periods
                        add_shades(periods_df2) |> #raw_periods
                        #add_shades(excluded_events, color = "#BDBDBD") %>%
                        add_labels_hmm(labels, labelLoc = 'bottom') |> #results$events
                        dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) |>
                        dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) |>
                        dygraphs::dyUnzoom()
     # }

  })

  output$hmm_covar <- renderPlot({

   req(trap_data())
    mv_data <- trap_data()$running
    mv_data$state <- factor(mv_data$state, levels = c(1, 2))

    ggplot2::ggplot(mv_data)+
      geom_point(aes(x = index, y = covar_smooth, color = state), alpha = 0.5)+
      scale_color_manual(values = c("#1B9E77", "#D95F02"))+
      ggtitle('Smoothed Covariance + HMM State')+
      ylab('Covariance')+
      xlab('Datapoint Time (Hz)')+
      theme_linedraw(base_size = 18)+
      theme(legend.position = 'none')


  })

  output$hmm_state <- renderPlot({

   req(trap_data())
    mv_data <- trap_data()$running
    mv_data$state <- factor(mv_data$state, levels = c(1, 2))

    mv1 <- ggplot2::ggplot(mv_data)+
              geom_point(aes(x = run_mean_1, y = covar_smooth, color = state), size = 3, alpha = 0.5)+
              scale_color_manual(values = c("#1B9E77", "#D95F02"))+
              ggtitle('M-CV Bead 1')+
              ylab('Covariance')+
              xlab('Mean (nm)')+
              theme_linedraw(base_size = 18)+
              theme(legend.position = 'none')

    mv2 <- ggplot(mv_data)+
              geom_point(aes(x = run_mean_2, y = covar_smooth, color = state), size = 3, alpha = 0.5)+
              scale_color_manual(values = c("#1B9E77", "#D95F02"))+
              ## facet_wrap(~state)+
              ggtitle('M-CV Bead 2')+
              ylab('Covariance')+
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
      nrow(trap_data()$events[keep_1 == T & keep_2 == T & event_user_excluded == FALSE]),
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
                        median_w_width = 200,
                        em_random_start = FALSE,
                        front_cp_method = "Mean/Var",
                        back_cp_method = "Mean/Var",
                        merge_threshold_dp = 0
                        ## cp_running_var_window = 5
                        )

    observeEvent(input$set_options, {
      a$w_width <- input$w_width
      a$w_slide <- input$w_slide
      ## a$median_w_width <- input$median_w_width
      a$em_random_start <- input$em_random_start
      a$front_cp_method <-input$front_cp_method
      a$back_cp_method <-input$back_cp_method
      a$merge_threshold_dp <- input$merge_threshold_dp
      ## a$cp_running_var_window <- input$cp_running_var_window
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
                            numericInput(ns("w_width"), "Covariance Window Width",
                                         value = a$w_width,
                                         width = "100%")
                            ),
                     column(6,
                            shinyWidgets::sliderTextInput(ns("w_slide"),
                                                          "Slide Window", c("1-Pt", "1/4", "1/2", "3/4", "No-overlap"),
                                                          grid = TRUE,
                                                          selected = a$w_slide,
                                                          width = "100%")
                            ## numericInput(ns("median_w_width"), "Running median window Width",
                            ##              value = a$median_w_width,
                            ##              width = "100%")
                            )
                   ) ,
                   fluidRow(
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
                   )

                   ),
                                        #cp tab panel
          tabPanel("Merge Events",
                   fluidRow(
                     column(6,
                            numericInput(ns("merge_threshold_dp"), label = "Merge event threshold (dp)", value = 0)
                            )
                   )
                   )
        )
      )
    )
  })

    #### Review Options ####

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
             fluidRow(
               plotOutput(ns("export_ggplot"), height = "175px")
             )
        )
      )
    })

    observeEvent(input$set_review_options, {
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

      if(input$save_as_gg){
        saveRDS(gg, file = paste0(file_name, ".rds"))
      } else {
        cowplot::ggsave2(file = paste0(file_name, ".pdf"),
                         plot = gg,
                         height =  as.numeric(input$export_plot_height),
                         width = as.numeric(input$export_plot_width),
                         units = "in")
      }
      showNotification("Plot Saved", type = "message")
      removeModal()
    })

    output$numbers <- renderPlot({

      req(trap_data())
      measured_events_data <- data.table::copy(trap_data()$events)
      measured_events_data[, displacement_nm := (displacement_bead_1_nm + displacement_bead_2_nm)/2 ]
      measured_events_data[, time_on_s := (attachment_duration_bead_1_s + attachment_duration_bead_2_s)/2 ]
      measured_events_data[, step1 := (substep_1_bead_1_nm + substep_1_bead_2_nm)/2 ]
      measured_events_data[, step2 := (substep_2_bead_1_nm + substep_2_bead_2_nm)/2 ]
      measured_events_data <- measured_events_data[keep_1 == TRUE & keep_2 == TRUE & event_user_excluded == FALSE]
      step <-
        ggplot()+
        stat_ecdf(data = measured_events_data, aes(displacement_nm), color = "red", pad = F)+
        stat_ecdf(aes(rnorm(1000,
                            mean = mean(measured_events_data$displacement_nm),
                            sd = sd(measured_events_data$displacement_nm))),
                  linetype = "dashed")+
        geom_vline(aes(xintercept = mean(measured_events_data$displacement_nm)), linetype = "dashed")+
        geom_label(aes(mean(measured_events_data$displacement_nm),
                       y = 0.9,
                       label = paste0("bar(x)==", round(mean(measured_events_data$displacement_nm), 1))),
                   parse = TRUE,
                   size = 6)+
        ggtitle("Total Displacements")+
        xlab("nm")+
        ylab('ECDF')+
        # scale_x_continuous(breaks = sort(c(seq(-100, 100, by = 20), round(mean(measured_events$displacement_nm), 1))))+
        cowplot::theme_cowplot(14)
        ## theme(
        ##   axis.text.y = element_blank(),
        ##   axis.line.y = element_blank(),
        ##   axis.ticks.y = element_blank()
        ## )

      step1 <-
        ggplot()+
        stat_ecdf(data = measured_events_data, aes(step1), color = "red", pad = F)+
        stat_ecdf(aes(rnorm(1000,
                            mean = mean(measured_events_data$step1),
                            sd = sd(measured_events_data$step1))),
                  linetype = "dashed")+
        geom_vline(aes(xintercept = mean(measured_events_data$step1, na.rm = TRUE)), linetype = "dashed")+
        geom_label(aes(mean(measured_events_data$step1, na.rm = TRUE),
                       y = 0.9,
                       label = paste0("bar(x)==", round(mean(measured_events_data$step1, na.rm = TRUE), 1))),
                   parse = TRUE,
                   size = 6)+
        ggtitle("Substep 1")+
        xlab("nm")+
        ylab('ECDF')+
        # scale_x_continuous(breaks = sort(c(seq(-100, 100, by = 20), round(mean(measured_events$displacement_nm), 1))))+
        cowplot::theme_cowplot(14)
        ## theme(
        ##   axis.text.y = element_blank(),
        ##   axis.line.y = element_blank(),
        ##   axis.ticks.y = element_blank()
        ## )

      step2 <-
        ggplot()+
        stat_ecdf(data = measured_events_data, aes(step2), color = "red", pad = F)+
        stat_ecdf(aes(rnorm(1000,
                            mean = mean(measured_events_data$step2),
                            sd = sd(measured_events_data$step2))),
                  linetype = "dashed")+
        geom_vline(aes(xintercept = mean(measured_events_data$step2, na.rm = TRUE)), linetype = "dashed")+
        geom_label(aes(mean(measured_events_data$step2, na.rm = TRUE),
                       y = 0.4,
                       label = paste0("bar(x)==", round(mean(measured_events_data$step2, na.rm = TRUE), 1))),
                   parse = TRUE,
                   size = 6)+
        ggtitle("Substep 2")+
        xlab("nm")+
        ylab('ECDF')+
        # scale_x_continuous(breaks = sort(c(seq(-100, 100, by = 20), round(mean(measured_events$displacement_nm), 1))))+
        cowplot::theme_cowplot(14)
        ## theme(
        ##   axis.text.y = element_blank(),
        ##   axis.line.y = element_blank(),
        ##   axis.ticks.y = element_blank()
        ## )

      time_on <-
        ggplot()+
        stat_ecdf(data = measured_events_data, aes(time_on_s), color = "red", pad = F)+
        geom_vline(aes(xintercept = median(measured_events_data$time_on_s)), linetype = "dashed")+
        geom_label(aes(x = median(measured_events_data$time_on_s),
                       y = 0.9,
                       label = paste0("Med. = ", round(median(measured_events_data$time_on_s*1000), 0))),
                   size = 6)+
        ggtitle("Time On")+
        xlab("ms")+
        ylab('ECDF')+
        cowplot::theme_cowplot(14)


      cowplot::plot_grid(step, step1, step2, time_on, nrow = 1)
    })

}
    
## To be copied in the UI
# mod_covariance_ui("covariance_1")
    
## To be copied in the server
# mod_covariance_server("covariance_1")
