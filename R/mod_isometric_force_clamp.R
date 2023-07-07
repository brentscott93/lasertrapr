#' isometric_force_clamp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_isometric_force_clamp_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, title = "Selected Folders",
          verbatimTextOutput(ns('selected_folders'))
      )),
    fluidRow(
      column(4,

             box(width = NULL, title = 'Isometric Force Clamp Analysis',
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
        DT::DTOutput(ns('table')) |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
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
                 plotOutput(ns('mv_by_state')) |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
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
        dygraphs::dygraphOutput(ns('overlay_dygraph'), height = '400px') |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
    )
      )
    # ) #rowclose
    #   ) #results box close
    )
  )#taglist clost
}
    
#' isometric_force_clamp Server Functions
# '@noRd
mod_isometric_force_clamp_server <- function(input, output, session, f){
    ns <- session$ns
    hm <- reactiveValues(analyze = 0)

  observeEvent(input$analyze_trap, {
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
      trap_data <- lapply(file$path, data.table::fread, nrows = 1)
    } else {
      files <- list_files(f$date$path, pattern = 'trap-data.csv', recursive = T)
      trap_data <- lapply(files$path, data.table::fread, nrows = 1)
    }

    opt <- reactiveValuesToList(isolate(a))
    withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
    purrr::walk(trap_data, ~isometric_force_clamp_analysis(
                                    trap_data = .x,
                                    f = f,
                                    w_width = a$w_width,
                                    w_slide = a$w_slide,
                                    em_random_start = a$em_random_start,
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

    filenames <- c('trap-data.csv', 'ifc-measured-events.csv', 'hm-model-data.csv', 'options.csv')
    paths <- purrr::map(filenames, ~list_files(f$obs$path, pattern = .x))
    data <-  purrr::map(paths, ~data.table::fread(.x$path))
    names(data) <- c('trap', 'events', 'running', 'options')
    data
  })


  output$overlay_dygraph <- dygraphs::renderDygraph({
    ## browser()
    req(trap_data())
    hz <- trap_data()$options$hz[[1]]
    feedback_motor_bead <- as.integer(trap_data()$options$feedback_motor_bead[[1]])

    if(feedback_motor_bead == 1){

    d <- data.table::data.table(index = (1:nrow(trap_data()$trap)/hz),
                                motor_bead = trap_data()$trap$processed_bead_1,
                                transducer_bead = trap_data()$trap$processed_bead_2
                                ## model = trap_data()$trap$hm_overlay
                                )

    } else if(feedback_motor_bead == 2) {

    d <- data.table::data.table(index = (1:nrow(trap_data()$trap)/hz),
                                motor_bead = trap_data()$trap$processed_bead_2,
                                transducer_bead = trap_data()$trap$processed_bead_1
                                ## model = trap_data()$trap$hm_overlay
                                )
    }

    periods_df <- data.table::data.table(start = trap_data()$events$cp_event_start_dp/hz,
                                         stop = trap_data()$events$cp_event_stop_dp/hz,
                                         keep = trap_data()$events$keep,
                                         event_user_excluded = trap_data()$events$event_user_excluded,
                                         color = scales::alpha("#D95F02" , 0.4))


   periods_df <- periods_df[keep == TRUE & event_user_excluded == FALSE]

    # get the peak nm index to put the labels here
   pni <-  trap_data()$events$peak_nm_index

    # add a column providiing the real event number
    # so when user filters out events, the events retain their real event number
    # making it easier to pick other events to exclude
   labels <- trap_data()$events[keep == TRUE & event_user_excluded == FALSE]
   str(labels)
   labels$peak_nm_index <- labels$midpoint_index_dp/hz


    overlay_dy <-
      dygraphs::dygraph(d) |> #raw_data_dygraph
      dygraphs::dySeries('motor_bead', color = "black") |>
      dygraphs::dySeries('transducer_bead', color = 'black') |>
      dygraphs::dyAxis("y", valueRange = c(min(d$transducer_bead)-100, max(d$motor_bead)+10)) |>
    ## dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) %>%
      dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') |>
      add_shades(periods_df) |>#raw_periods
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
    ggplot(mv_data)+
      geom_point(aes(x = index,
                     y = run_mean_motor,
                     color = state),
                 alpha = 0.5,
                 shape = 16)+
      scale_color_manual(values = c("#1B9E77", "#D95F02"))+
      ylab("Position (nm)")+
      xlab("Running Window Time Index (dp)")+
      cowplot::theme_cowplot(18)

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
      nrow(trap_data()$events[keep == TRUE]),
      "Events",
      icon = icon("slack-hash"),
      color = 'yellow'
    )

  })


  output$s2n <- renderValueBox({
    req(trap_data())
    valueBox(
      round(max(trap_data()$events[keep == TRUE]$force_pn), 2),
      "Max Force",
      icon = icon("dumbbell"),
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
    info()[,.('Obs' = obs,
                    'Include' = include,
                    'Analyzer' = analyzer,
                    #'Status' = status,
                    'Report' = report,
                    'Review' = review
              )] |>
      DT::datatable() |>
      DT::formatStyle('Include',
                  color = DT::styleEqual(c(F, T), c('red', 'black'))
                  ) |>
      DT::formatStyle('Report',
                  color = DT::styleEqual(c('error', 'success'), c('red', 'black'))
      ) |>
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
      trap$review <- input$quality_control

      data.table::fwrite(trap, file = file.path(f$obs$path, 'options.csv'))
      setProgress(1, detail = 'Done')
    })
     showNotification('Review saved' , type = 'message')
     shinyjs::click('info_table')
  })



    a <- reactiveValues(w_width = 2000,
                        w_slide = "1/2",
                        em_random_start = FALSE,
                        displacement_type = "avg")

    observeEvent(input$set_options, {
      a$w_width <- input$w_width
      a$w_slide <- input$w_slide
      a$em_random_start <- input$em_random_start
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
                                numericInput(ns("w_width"), "Window Width", value = 2000, min = 1, max = 50000),
                                ## sliderInput(ns("w_width"), "Window Width", min = 1, max = , value = a$w_width, width = "100%",step = 5)
                         ),
                         column(6,
                                shinyWidgets::sliderTextInput(ns("w_slide"),
                                                              "Slide Window", c("1-Pt", "1/4", "1/2", "3/4", "No-overlap"),
                                                              grid = TRUE,
                                                              selected = a$w_slide,
                                                              width = "100%")
                         )
                       ),
                        fluidRow(
                       ##   column(6,
                       ##          shinyWidgets::prettyRadioButtons(
                       ##            inputId = ns("use_channels"),
                       ##            label = "Channels",
                       ##            choices = c("Mean/Var", "Variance"),
                       ##            selected = a$use_channels,
                       ##            inline = TRUE,
                       ##            status = "primary",
                       ##            fill = TRUE
                       ##          )
                       ##      ),
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
        ) #hm-model tab close
        ## tabPanel("Changepoint",
        ##          fluidRow(
        ##            column(6,
        ##                   h4("Front"),
        ##                   shinyWidgets::prettyRadioButtons(
        ##                     inputId = ns("front_cp_method"),
        ##                     label = "Channels",
        ##                     choices = c("Mean/Var", "Variance"),
        ##                     selected = a$front_cp_method,
        ##                     inline = TRUE,
        ##                     status = "primary",
        ##                     fill = TRUE
        ##                   )
        ##            ),
        ##                   column(6,
        ##                          h4("Back"),
        ##                          shinyWidgets::prettyRadioButtons(
        ##                            inputId = ns("back_cp_method"),
        ##                            label = "Channels",
        ##                            choices = c("Mean/Var", "Variance"),
        ##                            selected = a$back_cp_method,
        ##                            inline = TRUE,
        ##                            status = "primary",
        ##                            fill = TRUE
        ##                          )
        ##                   )
        ##            ),
        ##             sliderInput(ns("cp_running_var_window"), "Running Variance Window Width", min = 5, max = 100, value = a$cp_running_var_window, width = "100%")
        ##   ), #cp tab panel
        ## tabPanel("Displacements",
        ##          radioButtons(inputId = ns('displacement_type'),
        ##                       label = 'Displacement Calculation Method',
        ##                       choices = list("Average" = "avg",
        ##                                      "Peak" = "peak"),
        ##                       inline = TRUE,
        ##                       selected = a$displacement_type)
        ##  )
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

               column(2,
                     numericInput(ns("bead_offset_1"),
                                  label = "Bead Offset 1",
                                  value = 0 )
               ),

               column(2,
                     numericInput(ns("bead_offset_2"),
                                  label = "Bead Offset 1",
                                  value = 0 )
               ),
               column(2,

                      numericInput(ns("export_plot_height"),
                                  label = "Height (in)",
                                  min = 0,
                                  max = 20,
                                  step = 0.5,
                                  value = 2.5),
               ),
               column(2,
                      numericInput(ns("export_plot_width"),
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
      if(!rlang::is_empty(input$exclude_event_manual)){
       excluded_data <- trap_data()$events
       if(input$include_exclude == "exclude"){
       excluded_data$event_user_excluded[as.numeric(input$exclude_event_manual)] <- TRUE
       } else if(input$include_exclude == "include"){
       excluded_data$event_user_excluded[as.numeric(input$exclude_event_manual)] <- FALSE
       }
       data.table::fwrite(excluded_data, file = file.path(f$obs$path, 'ifc-measured-events.csv'))
       showNotification(paste0("The following events were modified: ", toString(input$exclude_event_manual), " - Refreshing..."), duration = 3, type = "message")
      }
      removeModal()
      shinyjs::click("view_results")
    })

    observe({
    output$export_ggplot <- renderPlot(height = 175, {
      plot_overlay(obs_path = f$obs$path,
                   time_period_dp = input$overlay_dygraph_date_window,
                   color = input$export_plot_event_color,
                   bead_offset = c(input$bead_offset_1, input$bead_offset_2))
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
                         color = input$export_plot_event_color,
                         bead_offset = c(input$bead_offset_1, input$bead_offset_2))

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
      measured_events <- trap_data()$events[keep == TRUE & event_user_excluded == FALSE]
      to <-
        ggplot()+
        stat_ecdf(data = measured_events,
                     aes(x = time_on_ms),
                  size = 1,
                  pad = FALSE)+
        ggtitle("Time on")+
        xlab("Time (ms)")+
        ylab('ECDF')+
        # scale_x_continuous(breaks = sort(c(seq(-100, 100, by = 20), round(mean(measured_events$displacement_nm), 1))))+
        cowplot::theme_cowplot(18)+
        theme(
          ## axis.text.y = element_blank(),
          ## axis.line.y = element_blank(),
          ## axis.ticks.y = element_blank()
        )


      to_vs_force <-
        ggplot()+
        geom_point(data = measured_events,
                   aes(x = force_pn,
                       y = time_on_ms),
                   alpha = 0.5,
                   shape = 16)+
        ggtitle("Force Vs. Time On")+
        xlab("Force (pN)")+
        ylab("Time (ms)")+
        cowplot::theme_cowplot(18)+
        theme(
          ## axis.text.y = element_blank(),
          ## axis.line.y = element_blank(),
          ## axis.ticks.y = element_blank()
        )


      cowplot::plot_grid(to, to_vs_force)
    })

 
}
    
## To be copied in the UI
# mod_isometric_force_clamp_ui("isometric_force_clamp_1")
    
## To be copied in the server
# mod_isometric_force_clamp_server("isometric_force_clamp_1")
