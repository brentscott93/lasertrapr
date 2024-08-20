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
                 ## numericInput(ns("hz"), "Sampling Frequency", value = 5000, min = 0, max = 20000),
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
      box(width = NULL,

          title = 'Plots',
          # The id lets us use input$tabset1 on the server to find the current tab
          id = ns("distributions"),
           plotOutput(ns('summary_plots'), height = '400px') |> shinycssloaders::withSpinner(type = 8, color = '#373B38'),
          ## tabPanel("Displacements", plotOutput(ns('step'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("Force",  plotOutput(ns('force'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("Time On",  plotOutput(ns('ton'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("Time Off", plotOutput(ns('toff'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("ECDF", plotOutput(ns('ecdf'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## #tabPanel("Event Frequency", plotOutput(ns('ef'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("Stiffness", plotOutput(ns('stiffness'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          ## tabPanel("Correlations", plotOutput(ns('correlations'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'))
      )
    )
    )
    )
}

#' summarize Server Function
#'
#' @noRd
#' @import data.table cowplot ggplot2
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
    list_dir(f$project$path) |>
      dplyr::filter(str_detect(name, "summary", negate = TRUE)) |>
      dplyr::pull(name)
  })

 analyzer <- reactive({
   req(f$project$path)

   opt_path <- list.files(f$project$path,
                          pattern = "options.csv",
                          recursive = TRUE,
                          full.names = TRUE)

   opt <- data.table::rbindlist(lapply(opt_path, data.table::fread), fill = TRUE)
   opt <- opt[include == TRUE & review == TRUE & report == "success"]

   unique(opt$analyzer)

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
   req(analyzer())
   if(analyzer() == "ifc"){
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
                                                                                    value = .y))),
         numericInput(ns("fmin"), label = "Fmin (pN)", value = -1),
         numericInput(ns("ifc_tmin_ms"), label = "Tmin (ms)", value = 0)
       )
     } else {
       tagList(
         div(style = 'display:inline-block', colourpicker::colourInput(ns('color1'),
                                                                       label = 'Color 1',
                                                                       value = colorz())),

         numericInput(ns("fmin"), label = "Fmin (pN)", value = -1),
         numericInput(ns("ifc_tmin_ms"), label = "Tmin (ms)", value = 0)
       )
     }

   } else {
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
   ## browser()
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    withProgress(message = 'Summarizing Project', {
      golem::print_dev("before sum")
      
      summary_folder <- file.path(path.expand("~"), "lasertrapr", f$project_input, "summary")
      if(!dir.exists(summary_folder)){
        dir.create(summary_folder)
      }

      opt_path <- list.files(f$project$path,
                            pattern = "options.csv",
                            recursive = TRUE,
                            full.names = TRUE)

      opt <- data.table::rbindlist(lapply(opt_path, data.table::fread), fill = TRUE)
      opt <- opt[include == TRUE & review == TRUE & report == "success"]

      is_unique <- length(unique(opt$analyzer))==1
      if(!is_unique){
        showNotification("Data are not all analyzed with the same analyzer.", type = "error")
      }
      req(is_unique)

      plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
      analyzer <- unique(opt$analyzer)
      rv$analyzer <- analyzer
      if(analyzer %in% c("hm/cp", "mini")){
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
      
      purrr::walk2(list(all_measured_events, summary_data),
            c("all-measured-events.csv", "summary-data.csv"),
           ~ data.table::fwrite(.x, 
                               file.path(summary_folder, 
                                         paste(Sys.Date(), 
                                               f$project$name, 
                                               .y, 
                                               sep = "_"))))
      
      rv$all_measured_events <- copy(all_measured_events)
      rv$summary_data <- summary_data
      golem::print_dev("before colors")
## browser()
      setProgress(0.6)
      ggstep <- plot_ecdf(all_measured_events,
                          var = "displacement_nm",
                          colorz = plot_colors,
                          x_lab = "nanometers",
                          title = "Displacements",
                          basesize = 18)

      rv$ggstep <- ggstep

      setProgress(0.7)
      ggforce <- plot_ecdf(all_measured_events,
                           var = "force",
                           colorz = plot_colors,
                           x_lab = "piconewtons",
                           title = "Forces",
                           basesize = 18)
      rv$ggforce <- ggforce
      setProgress(0.8)
      ## ggton <- plot_ecdf(all_measured_events,
      ##                    var = "time_on_ms",
      ##                    colorz = plot_colors,
      ##                    x_lab = "milliseconds",
      ##                    title = "Time On",
      ##                    basesize = 18)
## browser()
      all_measured_events$time_on_s <- all_measured_events$time_on_ms/1000
      me_fit_ton <- fit_attachment_durations(all_measured_events, colorz = plot_colors)

      ggton <- me_fit_ton$gg
      rv$ton_rate <- me_fit_ton$rate


      ## rv$ggton <- ggton
      setProgress(0.9)
      all_measured_events$time_off_s <- all_measured_events$time_off_ms/1000
      me_fit_toff <- fit_time_off(all_measured_events, colorz = plot_colors)

      ggoff <- me_fit_toff$gg
      rv$toff_rate <- me_fit_toff$rate
      ## ggoff <- plot_ecdf(all_measured_events,
      ##                    var = "time_off_ms",
      ##                    colorz = plot_colors,
      ##                    x_lab = "milliseconds",
      ##                    title = "Time Off",
      ##                    basesize = 18)
      ## rv$ggoff <- ggoff

      rv$summary_plots <- cowplot::plot_grid(ggstep, ggforce, ggton, ggoff, nrow = 1)

     } else if(analyzer == "covar"){
      if(input$split_conditions){
        variables <- strsplit(conditions(), "_")
        number_variables <- sapply(variables, length)
        var_names <- purrr::map_chr(paste0('var', seq_len(number_variables[[1]])), ~input[[.x]])

        all_measured_events <- rbind_measured_events(project = f$project$name, save_to_summary = FALSE, n_channels = 2)
        all_measured_events <- split_conditions_column(all_measured_events, var_names = var_names, sep = "_")

        summary_data <- summarize_trap(all_measured_events, by = c("conditions", var_names), n_channels = 2)

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
        all_measured_events <- rbind_measured_events(project = f$project_input, save_to_summary = TRUE, n_channels = 2)
        summary_data <- summarize_trap(all_measured_events, by = "conditions", n_channels = 2)
      }

      ## tt <- total_trap_time(f$project_input, is_shiny = TRUE)

      ## summary_data <- merge(summary_data, tt, by = "conditions", all = TRUE)

      purrr::walk2(list(all_measured_events, summary_data),
            c("all-measured-events.csv", "summary-data.csv"),
           ~ data.table::fwrite(.x,
                               file.path(summary_folder,
                                         paste(Sys.Date(),
                                               f$project$name,
                                               .y,
                                               sep = "_"))))

    all_measured_events[, displacement_nm := (displacement_bead_1_nm + displacement_bead_2_nm)/2 ]
    all_measured_events[, time_on_s := (attachment_duration_bead_1_s + attachment_duration_bead_2_s)/2 ]
    all_measured_events[, step1 := (substep_1_bead_1_nm + substep_1_bead_2_nm)/2 ]
    all_measured_events[, step2 := (substep_2_bead_1_nm + substep_2_bead_2_nm)/2 ]

      rv$all_measured_events <- all_measured_events
      rv$summary_data <- summary_data
      golem::print_dev("before colors")
      setProgress(0.6)
      ggstep <- plot_ecdf(all_measured_events,
                          var = "displacement_nm",
                          colorz = plot_colors,
                          x_lab = "nanometers",
                          title = "Total Displacement",
                          basesize = 18)

      rv$ggstep <- ggstep

      ggstep1 <- plot_ecdf(all_measured_events,
                          var = "step1",
                          colorz = plot_colors,
                          x_lab = "nanometers",
                          title = "Substep 1",
                          basesize = 18)

      ggstep2 <- plot_ecdf(all_measured_events,
                          var = "step2",
                          colorz = plot_colors,
                          x_lab = "nanometers",
                          title = "Substep 2",
                          basesize = 18)

      setProgress(0.7)
      ## ggforce <- plot_ecdf(all_measured_events,
      ##                      var = "force",
      ##                      colorz = plot_colors,
      ##                      x_lab = "piconewtons",
      ##                      title = "Forces",
      ##                      basesize = 18)
      ## rv$ggforce <- ggforce
      setProgress(0.8)


      me_fit_ton <- fit_attachment_durations(all_measured_events, colorz = plot_colors)

      ggton <- me_fit_ton$gg
      rv$ton_rate <- me_fit_ton$rate


      ## ggton <- plot_ecdf(all_measured_events,
      ##                    var = "time_on_ms",
      ##                    colorz = plot_colors,
      ##                    x_lab = "milliseconds",
      ##                    title = "Time On",
      ##                    basesize = 18)

      rv$ggton <- ggton
      setProgress(0.9)
      ## ggoff <- plot_ecdf(all_measured_events,
      ##                    var = "time_off_ms",
      ##                    colorz = plot_colors,
      ##                    x_lab = "milliseconds",
      ##                    title = "Time Off",
      ##                    basesize = 18)
      ## rv$ggoff <- ggoff

      rv$summary_plots <- cowplot::plot_grid(ggstep, ggstep1, ggstep2, ggton, nrow = 1)

} else if(analyzer == "ifc") {

      if(input$split_conditions){
        variables <- strsplit(conditions(), "_")
        number_variables <- sapply(variables, length)
        var_names <- purrr::map_chr(paste0('var', seq_len(number_variables[[1]])), ~input[[.x]])

        all_measured_events <- rbind_measured_events(project = f$project$name, save_to_summary = FALSE)
        ## currently doing nothing with split columns in ifc summarize,
        ## but leaving here for future possibilites
        all_measured_events <- split_conditions_column(all_measured_events, var_names = var_names, sep = "_")
        all_measured_events <- all_measured_events[keep == TRUE & event_user_excluded == FALSE]

        all_measured_events <- all_measured_events[force_pn >= input$fmin]

        if(!is.null(input$factor_order)){
          all_measured_events$conditions <- factor(all_measured_events$conditions, levels = input$factor_order)
        }

        ## print(paste0("tmin = "), input$ifc_tmin_ms)
        clamp_fit <- fit_force_clamp(measured_events = all_measured_events,
                              tmin = input$ifc_tmin_ms/1000,
                              plot_colors = plot_colors,
                              basesize = 20,
                              textsize = 18,
                              is_shiny = TRUE)

        rv$clamp_fit <- clamp_fit
        ## summary_data <- summarize_trap(all_measured_events, by = c("conditions", var_names))

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
        all_measured_events <- all_measured_events[keep == TRUE & event_user_excluded == FALSE]


        if(!is.null(input$factor_order)){
          all_measured_events$conditions <- factor(all_measured_events$conditions, levels = input$factor_order)
        }


        all_measured_events <- all_measured_events[force_pn >= input$fmin]

        clamp_fit <- fit_force_clamp(measured_events = all_measured_events,
                                     tmin = input$ifc_tmin_ms/1000,
                                     plot_colors = plot_colors,
                                     basesize = 20,
                                     textsize = 18,
                                     is_shiny = TRUE)
        rv$clamp_fit <- clamp_fit
      }
## browser()
  rv$summary_plots <- clamp_fit$plot
  ## saveRDS(clamp_fit, file.path(summary_folder,
  ##                              paste(Sys.Date(),
  ##                              f$project_input,
  ##                             "clamp-fit.rds",
  ##                                     sep = "_")
  ##                                    )
  ##         )
  ## rv$summary_data <- clamp_fit$data

}
    })
    showNotification("Summary data saved!")
  })

  observeEvent(input$export, ignoreInit = T,  {

    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")

    withProgress(message = 'Saving Report', {

      opt_path <- list.files(f$project$path,
                            pattern = "options.csv",
                            recursive = TRUE,
                            full.names = TRUE)

      opt <- data.table::rbindlist(lapply(opt_path, data.table::fread), fill = TRUE)
      opt <- opt[include == TRUE & review == TRUE & report == "success"]

      is_unique <- length(unique(opt$analyzer))==1
      if(!is_unique){
        showNotification("Data are not all analyzed with the same analyzer.", type = "error")
      }
      req(is_unique)

      plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
      analyzer <- unique(opt$analyzer)
      if(analyzer %in% c("hm/cp", "mini")){



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

      } else if(analyzer == "ifc"){

      figures_folder <- file.path(path.expand("~"), "lasertrapr", f$project_input, "summary", "figures")
      if(!dir.exists(figures_folder)){
        dir.create(figures_folder)
      }
        saveRDS(rv$summary_plots, file = file.path(f$project$path,
                                                   "summary",
                                                   "figures",
                                                   paste0(Sys.Date(), "_ifc-bell-plots.rds")))
        ## cowplot::ggsave2(rv$summary_plots, file = file.path(path.expand("~"), "lasertrapr", "summary", Sys.Date(),"_ifc-bell-plots.png")
        data.table::fwrite(rv$clamp_fit$boot_params, file.path(f$project$path,
                                                               "summary",
                                                               paste0(Sys.Date(), "_ifc-bootstrap-bell-parameters.csv")))

        data.table::fwrite(rv$clamp_fit$predict_df, file.path(f$project$path,
                                                               "summary",
                                                               paste0(Sys.Date(), "_ifc-predict-line.csv")))

        data.table::fwrite(rv$clamp_fit$ci_ribbon_df, file.path(f$project$path,
                                                               "summary",
                                                               paste0(Sys.Date(), "_ifc-ci-ribbon.csv")))


      }
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

     if(rv$analyzer %in% c("hm/cp", "mini")){

      req(rv$ton_rate)
      req(rv$toff_rate)

       toff_rate <- rv$toff_rate |> dplyr::rename(attach_rate = html_label)
       rate_df <- dplyr::full_join(rv$ton_rate, toff_rate)


       rate_df |>
         dplyr::full_join(rv$summary_data) |>
      dplyr::arrange(conditions) |>
      dplyr::select("Conditions" = conditions, 
                    "Step Size (nm)" = displacement_avg,
                    "SE Step Size" = displacement_se,
                    "Force (pN)" = force_avg,
                    "SE Force" = force_se,
                    "Detachment Rate (Hz)" = html_label,
                    "Attachment Rate (Hz)" = attach_rate,
                    ## "Time Off (ms)" = time_off_avg,
                    ## "SE Toff" = time_off_se,
                    "No. Events" = num_events,
                    "Minutes Collected" = minutes
      ) |>
      dplyr::mutate_if(is.numeric, ~round(.,digits = 2)) |>
      DT::datatable(
                extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2)
                ))
    } else if(rv$analyzer == "covar"){
      req(rv$ton_rate)
      ## browser()
       rate_df <- rv$ton_rate
         ## dplyr::mutate(ton_rate = round(as.numeric(ton_rate), 0),
                       ## CI = paste0("(-", boot_ci$k1_low[[1]], "/+", boot_ci$k1_up[[1]], ")")) |>
         ## dplyr::select(conditions, ton_rate,  CI)

       rate_df |>
         dplyr::full_join(rv$summary_data) |>
      dplyr::arrange(conditions) |>
      dplyr::select("Conditions" = conditions,
                    "Total Displacement (nm)" = displacement_avg,
                    "SD Step Size" = displacement_sd,
                    "Substep 1" = substep_1_avg,
                    "SD Substep 1" = substep_1_sd,
                    "Substep 2" = substep_2_avg,
                    "SD Substep 2" = substep_2_avg,
                    "Detachment Rate (Hz)" = html_label,
                    ## "CI" = CI,
                    "No. Events" = num_events
      ) |>
      dplyr::mutate_if(is.numeric, ~round(.,digits = 2)) |>
      DT::datatable(
                extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2)
                ))


    }
  })


 output$summary_plots <- renderPlot({
   req(rv$summary_plots)
   rv$summary_plots
 })
  ## output$step <- renderPlot({
  ##   req(rv$step)
  ##   rv$step
  ## })
  ## output$ton <- renderPlot({
  ##   req(rv$ton)
  ##   rv$ton
  ## })
  ## output$toff <- renderPlot({
  ##   req(rv$toff)
  ##   rv$toff
  ## })
  ## output$force <- renderPlot({
  ##   req(rv$force)
  ##   rv$force
  ## })
  ## output$ecdf <- renderPlot({
  ##   req(rv$ton_ecdf)
  ##   plot_grid(rv$ton_ecdf, rv$toff_ecdf)
  ## })
  ## output$ef <- renderPlot({
  ##   req(rv$ef)
  ##   rv$ef
  ## })
  ## output$correlations <- renderPlot({
  ##   req(rv$correlations)
  ##   rv$correlations
  ## })
  ## output$stiffness <- renderPlot({
  ##   req(rv$stiffness)
  ##   rv$stiffness
  ## })

}

## To be copied in the UI
# mod_summarize_ui("summarize")

## To be copied in the server
# callModule(mod_summarize_server, "summarize")

