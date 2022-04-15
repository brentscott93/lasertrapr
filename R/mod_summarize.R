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
                 shinyWidgets::materialSwitch(ns('split_conditions'), 
                                              'Split Conditions',
                                              value = FALSE, 
                                              status = 'primary'),
                 
                 conditionalPanel(condition = "input.split_conditions === true", ns = ns,
                                  uiOutput(ns('split_conditions_options')),             
                 ),
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
          fluidRow(
            column(12, 
                   DT::DTOutput(ns('table')) %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'),
                   br())
            )
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
          tabPanel("ECDF", plotOutput(ns('ecdf'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          #tabPanel("Event Frequency", plotOutput(ns('ef'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
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
#' @import data.table cowplot rstatix
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
      dplyr::filter(str_detect(name, "summary", negate = TRUE)) %>% 
      dplyr::pull(name)
  })
  
  colorz <- reactive({
    if(length(conditions()) == 1){
         "#002cd3"
    } else if(length(conditions()) == 2){
        c("#002cd3", "#d30000")
      } else {
        RColorBrewer::brewer.pal(length(conditions()), 'Set1')
      }
  })
  
  output$user_defaults <- renderUI({
    req(conditions())
    if(length(conditions()) >= 2){
    tagList(
    
      selectInput(ns('factor_order'),
                  label = 'Factor Order',
                  multiple = T,
                  choices = conditions()),
      purrr::map2(seq_along(conditions()),
                  colorz(),
                  ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
                                             label = paste('Color', .x),
                                             value = .y)))
          )
    } else {
      div(style = 'display:inline-block', colourpicker::colourInput(ns('color1'),
                                                                    label = 'Color 1',
                                                                    value = colorz()))
    }
  })
  
  output$split_conditions_options <- renderUI({
    req(conditions())
    variables <- strsplit(conditions(), "_")
    number_variables <- sapply(variables, length)
    var_equal <- var(number_variables) == 0
    defend_if(!var_equal, ui = "Cannot split conditions column. Un-even variables numbers in each conditions name", type = "error")
    tagList(
      purrr::map(seq_len(number_variables[[1]]),
                  ~div(style = 'display:inline-block', textInput(ns(paste0('var', .x)),
                                                                  label = paste('Variable', .x))))
    )
  })
  
  rv <- reactiveValues(summary = data.frame(x = letters),
                               save = 0)

  observeEvent(input$go, {
   # browser()
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    withProgress(message = 'Summarizing Project', {
      golem::print_dev("before sum")
      
      summary_folder <- file.path("~", "lasertrapr", f$project_input, "summary")
      if(!dir.exists(summary_folder)){
        dir.create(summary_folder)
      }
      
      if(input$split_conditions){
        variables <- strsplit(conditions(), "_")
        number_variables <- sapply(variables, length)
        var_names <- purrr::map_chr(paste0('var', seq_len(number_variables[[1]])), ~input[[.x]])
        
        all_measured_events <- rbind_measured_events(project = f$project$name, save_to_summary = FALSE)
        all_measured_events <- split_conditions_column(all_measured_events, var_names = var_names, sep = "_")
        
        summary_data <- summarize_trap(all_measured_events, by = c("conditions", var_names))
        
        sc <- split_conditions_column(data.frame(conditions=summary_data$conditions), var_names = var_names, sep = "_")
        data.table::fwrite(sc, 
                           file.path(summary_folder, 
                                     paste(Sys.Date(), 
                                           f$project_input, 
                                           "split-conditions.csv", 
                                           sep = "_")
                                     )
                                )
      } else {
        all_measured_events <- rbind_measured_events(project = f$project_input, save_to_summary = TRUE)
        summary_data <- summarize_trap(all_measured_events, by = "conditions")
      }
      
      tt <- total_trap_time(f$project_input, is_shiny = TRUE)
      
      summary_data <- merge(summary_data, tt, by = "conditions", all = TRUE)
      
      walk2(list(all_measured_events, summary_data), 
            c("all-measured-events.csv", "summary-data.csv"),
           ~ data.table::fwrite(.x, 
                               file.path(summary_folder, 
                                         paste(Sys.Date(), 
                                               f$project$name, 
                                               .y, 
                                               sep = "_"))))
      
      rv$all_measured_events <- all_measured_events
      rv$summary_data <- summary_data
      golem::print_dev("before colors")
      plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
      setProgress(0.6, detail = "Step Stats")
        rv$step <-  ggstatsplot::ggbetweenstats(rv$all_measured_events,
                       x = conditions, 
                       y = displacement_nm,
                       ylab = "nanometers",
                       xlab = "",
                       title = "Displacements",
                       ggplot.component = list(scale_color_manual(values = plot_colors)),
                       centrality.point.args = list(size = 5, color = "grey10"),
                       ggtheme = theme_cowplot())
      
      setProgress(0.65, detail = "Time On Stats")
      rv$ton <- ggstatsplot::ggbetweenstats(rv$all_measured_events,
                              x = conditions, 
                              y = time_on_ms,
                              ylab = "milliseconds",
                              xlab = "",
                              title = "Attachment Times",
                              centrality.point.args = list(size = 5, color = "grey10"),
                              ggtheme = theme_cowplot(),
                              type = "nonparametric",
                              ggstatsplot.layer = F,
                              ggsignif.args = list(step_increase = 1),
                              ggplot.component = list(scale_color_manual(values = plot_colors),
                                                      scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                                         labels = scales::trans_format("log10", scales::math_format(10^.x))),
                                                      coord_trans(y = "log10")))
      setProgress(0.7, detail = "Time Off Stats")
      rv$toff <- ggstatsplot::ggbetweenstats(rv$all_measured_events,
                               x = conditions, 
                               y = time_off_ms,
                               ylab = "milliseconds",
                               xlab = "",
                               title = "Time Between Events",
                               centrality.point.args = list(size = 5, color = "grey10"),
                               ggtheme = theme_cowplot(),
                               type = "nonparametric",
                               ggstatsplot.layer = F,
                               ggsignif.args = list(step_increase = 1),
                               ggplot.component = list(scale_color_manual(values = plot_colors),
                                                       scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                                          labels = scales::trans_format("log10", scales::math_format(10^.x))),
                                                       coord_trans(y = "log10")))
      setProgress(0.75, detail = "Force Stats")
        rv$force <- ggstatsplot::ggbetweenstats(rv$all_measured_events,
                                   x = conditions, 
                                   y = force,
                                   ylab = "piconewtons",
                                   xlab = "",
                                   title = "Forces",
                                   ggplot.component = list(scale_color_manual(values = plot_colors)),
                                   centrality.point.args = list(size = 5, color = "grey10"),
                                   ggtheme = theme_cowplot())
      setProgress(0.8, detail = "Time On ECDF")
      rv$ton_ecdf <- time_on_ecdf(event_files_filtered = rv$all_measured_events,
                                   plot_colors = plot_colors)
      setProgress(0.85, detail = "Time Off ECDF")
      rv$toff_ecdf <- time_off_ecdf(event_files_filtered = rv$all_measured_events,
                                  plot_colors = plot_colors)
      # setProgress(0.85, detail = "Event Frequency")
      # rv$ef <- stats_plot_event_frequency(event_file_path = rv$data$event_file_path, 
      #                                     factor_order = input$factor_order,
      #                                     plot_colors = plot_colors)
      setProgress(0.9, detail = "Correlations")
      # rv$correlations <- correlations(event_files_filtered = rv$all_measured_events,
      #                                 plot_colors = plot_colors)
      setProgress(0.95, detail = "Stiffness")
      # rv$stiffness <- stiffness(event_files_filtered = rv$all_measured_events,
      #                              plot_colors = plot_colors)
    })
    showNotification("Summary data saved!")
  })

  observeEvent(input$export, ignoreInit = T,  {
    withProgress(message = 'Saving Report', {
      temp_report <- file.path(tempdir(), "project-summary.Rmd")

      report_file <- system.file("rmd", "project-summary-flex.Rmd", package = "lasertrapr")

      file.copy(report_file, temp_report, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(rv = rv, 
                     factor_order = input$factor_order)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out_dir <- file.path(f$project$path, "summary")
      if(!file.exists(out_dir)) dir.create(out_dir)
      rmarkdown::render(temp_report, output_dir = out_dir,
                        params = params,
                        envir = new.env(parent = globalenv()))
    })
  })
  output$table <- DT::renderDT({
    validate(need('conditions' %in% colnames(rv$summary_data), 'Select completed project, choose options, and click summarize'))
    # rv$summary_data$conditions <- factor(summarize_trap$conditions,
    #                                     levels = input$factor_order)i
    
    if(!is.null(input$factor_order)){
     rv$summary_data$conditions <- 
        factor(rv$summary_data$conditions ,
               levels = input$factor_order)
    }
    rv$summary_data %>%
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
                ))
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
  output$ecdf <- renderPlot({
    req(rv$ton_ecdf)
    plot_grid(rv$ton_ecdf, rv$toff_ecdf)
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

