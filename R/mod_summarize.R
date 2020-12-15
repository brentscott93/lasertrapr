#' summarize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summarize_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             box(title = 'Controls',
                 width = NULL,
                 h4('Selected Project'),
                 verbatimTextOutput(ns('current_project')),
                 uiOutput(ns('user_defaults')),
                 numericInput(ns("hz"), "Sampling Frequency", value = 5000, min = 0, max = 20000),
                 actionButton(ns('go'),
                              'Summarize',
                              icon = icon('calculator'),
                              width = "49%",
                              style = "display: inline-block;"),
                 actionButton(ns('export'),
                              'Export',
                              icon = icon('file-export'),
                              width = "49%",
                              style = "display: inline-block;")
                 )
             ),
      column(9,
        box(width = NULL,
          title = 'Summary Table',
          # The id lets us use input$tabset1 on the server to find the current tab
          id = ns("project_summary"),

                  DT::DTOutput(ns('table')) %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'),

          )
        )
      ),
    fluidRow(
      column(12,
      tabBox(width = NULL,

          title = '',
          # The id lets us use input$tabset1 on the server to find the current tab
          id = ns("distributions"),
          tabPanel("Displacements", plotOutput(ns('step'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Force",  plotOutput(ns('force'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Time On",  plotOutput(ns('ton'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Time Off", plotOutput(ns('toff'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Ton Survival", plotOutput(ns('ton_survival'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Event Frequency", plotOutput(ns('ef'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Stiffness", plotOutput(ns('stiffness'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Correlations", plotOutput(ns('correlations'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'))
      )
    )
    )
  )
}

#' summarize Server Function
#'
#' @noRd
#' @import data.table ggpubr cowplot rstatix survival survminer patchwork
mod_summarize_server <- function(input, output, session, f){
 ns <- session$ns
  # 
  output$current_project <- renderText({
    validate(need(f$project$name, 'Select a project'))
    f$project$name
  })
  # 
  conditions <- reactive({
    req(f$project$path)
    list_dir(f$project$path) %>%
      dplyr::pull(name)
  })
  output$user_defaults <- renderUI({
    req(conditions())
    tagList(
      selectInput(ns('factor_order'),
                  label = 'Factor Order',
                  multiple = T,
                  choices = conditions()),
      purrr::map2(seq_along(conditions()),
                  RColorBrewer::brewer.pal(length(conditions()), 'Set1'),
                  ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
                                             label = paste('Color', .x),
                                             value = .y)))
    )
  })
  # 
  # 
  rv <- reactiveValues(summary = data.frame(x = letters),
                         save = 0)
  # 
  observeEvent(input$go, {
    golem::print_dev('summary go clicked')
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    withProgress(message = 'Summarizing Project', {
      rv$data <- summarize_trap_data(f = f,
                                     hz = input$hz,
                                     factor_order = input$factor_order)
      plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
      setProgress(0.6, detail = "Step Stats")
      rv$step <- stats_plot_step(event_files_filtered = rv$data$event_files_filtered,
                                 plot_colors = plot_colors)
      setProgress(0.65, detail = "Time On Stats")
      rv$ton <- stats_plot_time_on(event_files_filtered = rv$data$event_files_filtered,
                                       plot_colors = plot_colors)
      setProgress(0.7, detail = "Time Off Stats")
      rv$toff <- stats_plot_time_off(event_files_filtered = rv$data$event_files_filtered,
                                         plot_colors = plot_colors)
      setProgress(0.75, detail = "Force Stats")
      rv$force <- stats_plot_force(event_files_filtered = rv$data$event_files_filtered,
                                   plot_colors = plot_colors)
      setProgress(0.8, detail = "Time On Survival")
      rv$ton_survival <- survival_analysis(event_files_filtered = rv$data$event_files_filtered,
                                           plot_colors = plot_colors)
      setProgress(0.85, detail = "Event Frequency")
      rv$ef <- stats_plot_event_frequency(event_file_path = rv$data$event_file_path, 
                                          factor_order = input$factor_order,
                                          plot_colors = plot_colors)
      setProgress(0.9, detail = "Event Frequency")
      rv$correlations <- correlations(event_files_filtered = rv$data$event_files_filtered,
                                      plot_colors = plot_colors)
      setProgress(0.95, detail = "Stiffness")
      rv$stiffness <- stiffness(event_files_filtered = rv$data$event_files_filtered,
                                   plot_colors = plot_colors)
    })
  })

  observeEvent(input$export, ignoreInit = T,  {
    withProgress(message = 'Saving Report', {
      temp_report <- file.path(tempdir(), "project-summary.Rmd")

      report_file <- system.file("rmd", "project-summary-flex.Rmd", package = "lasertrapr")

      file.copy(report_file, temp_report, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(rv = rv)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(temp_report, output_dir = f$project$path,
                        params = params,
                        envir = new.env(parent = globalenv()))
    })
  })
  output$table <- DT::renderDT({
    validate(need('conditions' %in% colnames(rv$data$summary), 'Select completed project, choose options, and click summarize'))
    # rv$data$summary$conditions <- factor(summarize_trap$conditions,
    #                                     levels = input$factor_order)
    rv$data$summary %>%
      dplyr::arrange(conditions) %>%
      dplyr::select("Conditions" = conditions, 
                    "Step Size (nm)" = displacement_avg,
                    "SE Step Size" = displacement_se,
                    "Force (pN)" = force_avg,
                    "SE Force" = force_se, 
                    "Avg Time On (ms)" = time_on_avg,
                    "SE Ton" = time_on_se, 
                    'Median Time on (ms)' = time_on_median,
                    "Time Off (ms)" = time_off_avg,
                    "SE Toff" = time_off_se, 
                    "No. Events" = num_events,
                    "Minutes Collected" = minutes
      ) %>% 
      mutate_if(is.numeric, ~round(.,digits = 2)) %>% 
      DT::datatable(
                extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2)
                )
    )
  })
  output$step <- renderPlot({
    req(rv$step)
    rv$step
  })
  output$ton <- renderPlot({
    req(rv$ton)
    rv$ton
  })
  output$toff <- renderPlot({
    req(rv$toff)
    rv$toff
  })
  output$force <- renderPlot({
    req(rv$force)
    rv$force
  })
  output$ton_survival <- renderPlot({
    req(rv$ton_survival)
    rv$ton_survival
  })
  output$ef <- renderPlot({
    req(rv$ef)
    rv$ef
  })
  output$correlations <- renderPlot({
    req(rv$correlations)
    rv$correlations
  })
  output$stiffness <- renderPlot({
    req(rv$stiffness)
    rv$stiffness
  })

}

## To be copied in the UI
# mod_summarize_ui("summarize")

## To be copied in the server
# callModule(mod_summarize_server, "summarize")

