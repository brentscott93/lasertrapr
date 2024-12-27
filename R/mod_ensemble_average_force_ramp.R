#' ensemble_average_force_ramp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ensemble_average_force_ramp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        tabBox(width = 3,
               tabPanel(
               title = "Prep",
                 sliderInput(ns("ms_extend_s2"),
                             "Avg of ms to extend forward",
                             value = 2,
                             min = 1,
                             max = 10,
                             step = 1,
                             width = "100%"),
                 sliderInput(ns("ms_2_skip"),
                             "Number of ms to skip before s1 avg",
                             value = 2,
                             min = 1,
                             max = 10,
                             step = 1,
                             width = "100%"),
             sliderInput(ns("ms_extend_s1"),
                         "Avg of ms to extend s1 backwards",
                         value = 2,
                         min = 1,
                         max = 10,
                         step = 1,
                         width = "100%"),
                  shinyWidgets::radioGroupButtons(
                    inputId = ns('events_select'),
                    label = "Select events?",
                    choices = c("All" = "all",
                                "Positive" = "positive",
                                "Negative" = "negative"),
                    justified = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-check-square",
                                   style = "color: black"),
                      no = tags$i(class = "fa fa-square-o",
                                  style = "color: black"))
                    ),
             ## actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%", icon = icon("align-justify"))
             ),
             tabPanel(
               title = "Average",
        actionButton(ns("avg_ensembles"), "Avg Ensembles", width = "100%", style  = "margin-top: 25px", icon = icon("calculator")),
               ),
             tabPanel(
               title = "Plot Options",
              actionButton(ns("plot_ensembles"), "Plot Ensembles", width = "100%"),
               div(style = "diplay: inline-block;",
                   uiOutput(ns("ee_ui"))),
               uiOutput(ns("facet_col")),
               shinyWidgets::materialSwitch(ns("custom_labels"), "Custom Labels", status = "primary"),
               uiOutput(ns("ee_labels")),

               ## sliderInput(ns("backwards_shift"),
               ##             "Backwards Shift (seconds)",
               ##             min = -1,
               ##             max = 1,
               ##             step = 0.05,
               ##             value = 0),
               numericInput(ns("size"), "Theme Size", value = 14),
               actionButton(ns("save_plot"), "Save Plot", width = "100%")

             )


               ),

      ## uiOutput(ns("main_box")),

            tabBox(side = "right",
             width = 8,
              title = "Ensemble Averages",
             tabPanel(
                "Plot",
                plotOutput(ns("forward_backward_ensembles"), height = "auto") |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
             ),

            ),
            box(width = 1,
                   title = "Height",
                   shinyWidgets::noUiSliderInput(ns("plot_height"),
                                                 "",
                                                 min = 0,
                                                 max = 650,
                                                 value = 500,
                                                 tooltips = FALSE,
                                                 step = 5,
                                                 direction = "rtl",
                                                 orientation = "vertical",
                                                 color = "#444444",
                                                 width = "100px",
                                                 height = "600px"))
        ),
    fluidRow(
       column(3),
       column(9,
              box(width = NULL,
                  title = "Width",
                  shinyWidgets::noUiSliderInput(ns("plot_width"),
                                                "",
                                                min = 100,
                                                max = 950,
                                                value = 900,
                                                tooltips = FALSE,
                                                step = 5,
                                                color = "#444444",
                                                width = "100%" )
         )
      )
   )
)

}

    
#' ensemble_average_force_ramp Server Functions
#'
#' @noRd

mod_ensemble_average_force_ramp_server <- function(input, output, session, f, input_sidemenu){
  ns <- session$ns
  ee <- reactiveValues()

  observeEvent(f$project_input, {
    req(input_sidemenu)
    print("sidemenu-test")
str(input_sidemenu())
    if(input_sidemenu() == "ensemble_average"){
     req(f$project_input)
     req(f$project$path)

     options_paths <- list.files(path = f$project$path,
                                 pattern = "options.csv",
                                 full.names = TRUE,
                                 recursive = TRUE)

     options_data <-
        data.table::rbindlist(
           lapply(options_paths, data.table::fread),
           fill = TRUE
        )

     req(options_data)
     options_data <- options_data[include == TRUE & review == TRUE & report == "success"]

     ee$analyzer <- unique(options_data$analyzer)
     print(paste("ee$analyzer = ", ee$analyzer))
     if(length(ee$analyzer)>1){
     ee$analyzer <- NULL
     showNotification("Multiple analyzer detected. Unable to process")
     }
     print(ee$analyzer)
     str(options_data)
    }
  })

  output$main_box <- renderUI({
    ## req(ee$analyzer)
    if(is.null(ee$analyzer)) {
     tagList(h4("Data needs to be analyzed before ensemble averaging."))
    } else if(ee$analyzer == "hm/cp" || ee$analyzer == "covar"){

      tagList(
        tabBox(side = "right",
               width = 8,
               title = "Ensemble Averages",
               tabPanel(
                 "Plot",
                 plotOutput(ns("forward_backward_ensembles"), height = "auto") |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
               )
               )
      )

    } else {
     tagList(h4("Data needs to be analyzed before ensemble averaging."))
    }

  })

  observeEvent(input$prep_ensemble, {
   ## browser()

    golem::print_dev(f$project_input)
    golem::print_dev(str(f))
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")

    options_paths <- list.files(path = f$project$path,
                                pattern = "options.csv",
                                full.names = TRUE,
                                recursive = TRUE)

    options_data <-
          data.table::rbindlist(
             lapply(options_paths, data.table::fread),
             fill = TRUE
          )

    options_data <- options_data[include == TRUE & review == TRUE & report == "success"]
    ee$hz <- unique(options_data$hz)

    defend_if(length(ee$hz) != 1,
              ui = "Data has different sampling frequencies. Ensmeble averaging not currently supported.",
              type = "error")

    analyzer <- unique(options_data$analyzer)
    defend_if(length(analyzer) != 1,
              ui = "Multiple analyzers detected for trap data. Unable to ensemble average.",
              type = "error")

    withProgress(message = "Preparing Ensembles", {

      prep_ensemble_force_ramp(trap_selected_project = f$project$path,
                               ms_extend_s2 = input$ms_extend_s2,
                               ms_extend_s1 = input$ms_extend_s1,
                               ms_2_skip = input$ms_2_skip,
                               hz = ee$hz,
                               events_select = input$events_select)

    })
    showNotification("Ensembles Prepared", type= "message")
  })


   observeEvent(input$avg_ensembles, {
     defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
     defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")

     options_paths <- list.files(path = f$project$path,
                                 pattern = "options.csv",
                                 full.names = TRUE,
                                 recursive = TRUE)

     options_data <-
       data.table::rbindlist(
         lapply(options_paths, data.table::fread),
         fill = TRUE
       )

     options_data <- options_data[include == TRUE & review == TRUE & report == "success"]
     ee$hz <- unique(options_data$hz)

     defend_if(length(ee$hz) != 1,
               ui = "Data has different sampling frequencies. Ensmeble averaging not currently supported.",
               type = "error")

    analyzer <- unique(options_data$analyzer)
    defend_if(length(analyzer) != 1,
              ui = "Multiple analyzers detected for trap data. Unable to ensemble average.",
              type = "error")


     withProgress(message = "Averaging Ensembles", {
       ## browser()
       avg_ensembles_force_ramp(project = f$project_input)
       showNotification("Ensembles Averaged", type = "message")


     ee$data <- data.table::fread(file.path(f$project$path, "summary", "ensemble-averages.csv"))
     })
     })



   ## observeEvent(input$plot_ensembles, {
     ## ensemble_data <- data.table::fread(file.path(f$project$path, "summary", "ensemble-averages.csv"))

     ## req(ensemble_data)

     ## ee$plot <-
     ##   ggplot(ensemble_data)+
     ##   geom_point(aes(x = ensemble_index,
     ##                  y = avg,
     ##                  color = conditions))+
     ##   facet_grid(direction~conditions)+
     ##   scale_color_manual(values = ee$plot_colors)+
     ##   cowplot::theme_cowplot(input$size)

     ## })

   conditions <- reactive({
     req(f$project$path)
     list_dir(f$project$path) |>
       dplyr::filter(str_detect(name, "summary", negate = TRUE)) |>
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

   output$facet_col <- renderUI({
     req(conditions())
     if(length(conditions()) >= 2){
        shinyWidgets::radioGroupButtons(
           inputId = ns('nrow'),
           label =   "Number of Facet Rows",
           choices = 1:length(conditions()),
           justified = TRUE,
           checkIcon = list(
              yes = tags$i(class = "fa fa-check-square",
                           style = "color: black"),
              no = tags$i(class = "fa fa-square-o",
                          style = "color: black"))
        )
     }
   })
   output$ee_ui <- renderUI({
     req(conditions())
     if(length(conditions()) >= 2){
       tagList(
         selectInput(ns('factor_order'),
                     label = 'Factor Order',
                     multiple = T,
                     choices = conditions()),
         purrr::map2(seq_along(conditions()),
                     colorz(),
                     ~div(style = 'display:inline-block; width: 75px', colourpicker::colourInput(ns(paste0('color', .x)),
                                                                                    label = paste('Color', .x),
                                                                                    value = .y)))
       )
     } else {
       div(style = 'display:inline-block; ', colourpicker::colourInput(ns('color1'),
                                                                     label = 'Color 1',
                                                                     value = colorz()))
     }
   })

   output$ee_labels <- renderUI({
     req(input$custom_labels)
     if(input$custom_labels){
     purrr::map(seq_along(conditions()),
                ~div(style = 'display:inline-block; width: 75px;', textInput(ns(paste0('label', .x)), label = paste('Label', .x))))
     }
   })

 observe({
   req(ee$data)
   ## req(ee$forward_predict_df)
   req(input$color1)

   ee_data <- ee$data

   facet_nrow <- if(length(conditions()) >=2 ){
     req(input$nrow)
     as.numeric(input$nrow)
   } else {
     1
   }

   plot_colors <- purrr::map_chr(seq_along(conditions()), ~input[[paste0('color', .x)]])
   ee$plot_colors <- plot_colors

   if(!is.null(input$factor_order)){
     if(input$custom_labels){
      if(length(input$factor_order) == length(conditions())){
       if(nchar(input$factor_order[length(input$factor_order)]) > 0){
       req(input$label1)
       last <- input[[paste0('label', length(conditions()))]]
       if(last != ""){
       req(last)
       new_labels <- purrr::map_chr(seq_along(conditions()), ~input[[paste0('label', .x)]])
       ee_data$conditions <- factor(ee_data$conditions,
                                    levels = input$factor_order,
                                    labels = new_labels)

        }
       }
      }
     } else {

      if(length(input$factor_order) == length(conditions())){
       if(nchar(input$factor_order[length(input$factor_order)]) > 0){

       ee_data$conditions <- factor(ee_data$conditions,
                                    levels = input$factor_order)
     }
       }
       }
   }

   ## length_of_full_ensemble <- nrow(ee_data[direction=="forward" & ensemble_index >= 0])
   ## diff_in_length_filter <- length_of_full_ensemble - nrow(ee_forward_predict_df)
   hz <- ee$hz
   ## shift <- input$backwards_shift

   ## original_length <- nrow(ee_data[direction == "forward" & ensemble_index >= 0])
   ## max_forward_fit_index <- nrow(ee_forward_predict_df)

   ## ee_data_backwards_baseline[, time_shifted := time+2]

   ## if(max_forward_fit_index < original_length){
   ##   filter_diff <- original_length-max_forward_fit_index
   ##   ee_data_backwards[, forward_backward_index := forward_backward_index-(2*filter_diff)]
   ##   }
   ee_data$direction <- factor(ee_data$direction, levels = c("forward", "backwards"))

     ee$plot <-
       ggplot(ee_data)+
       geom_point(aes(x = ensemble_index,
                      y = avg,
                      color = conditions))+
       facet_grid(conditions~direction, scales = "free_x")+
       scale_color_manual(values = plot_colors)+
       cowplot::theme_cowplot(input$size)


   ## ee$plot <-
   ## ggplot()+
   ##   geom_point(data = ee_data_forward_baseline,
   ##              aes(x = ensemble_index/hz,
   ##                  y = avg,
   ##                  color = conditions),
   ##              alpha = 0.3,
   ##              shape = 16,
   ##              size = 0.8)+
   ##   geom_point(data = ee_backwards_baseline_df,
   ##              aes(x = time_shifted - shift,
   ##                  y = avg,
   ##                  color = conditions),
   ##              alpha = 0.3,
   ##              shape = 16,
   ##              size = 0.8)+
   ##   geom_point(data = ee_forward_fitted_raw_df,
   ##              aes(x = time,
   ##                  y = avg,
   ##                  color = conditions),
   ##              alpha = 0.3,
   ##              shape = 16,
   ##              size = 0.8)+
   ##   geom_point(data = ee_backwards_fitted_raw_df,
   ##              aes(x = time_shifted - shift,
   ##                  y = avg,
   ##                  color = conditions),
   ##              alpha = 0.3,
   ##              shape = 16,
   ##              size = 0.8)+
   ##   geom_line(data = ee_forward_predict_df,
   ##             aes(x = time,
   ##                 y = predict_y))+
   ##   geom_line(data = ee_backwards_predict_df,
   ##             aes(x = time_shifted - shift,
   ##                 y = predict_y))+
   ##   facet_wrap(~conditions, scales = "free_x", nrow = facet_nrow)+
   ##   ylab("nanometers")+
   ##   xlab("seconds")+
   ##   scale_color_manual(values = plot_colors)+
   ##   cowplot::theme_cowplot(input$size)+
   ##   theme(
   ##     strip.background = element_rect(fill = "transparent"),
   ##     legend.position = "none"
   ##   )
 })

 output$forward_backward_ensembles <- renderPlot({
   validate(need(ee$plot, "Prep & Average Ensembles"))
   ## validate(need(ee$fits,"Average Ensembles Before Continuing"))
   validate(need(input$color1, "Navigate to 'Plot' tab to view Ensemble Plots"))

  ee$plot

 }, height = function() input$plot_height, width = function() input$plot_width )


 observeEvent(input$save_plot, {

    filename <- paste0(gsub(":", "-", sub(" ", "_", Sys.time())), "_ensemble-average-plot")
    ee$size_ratio <- input$plot_width/input$plot_height
    showModal(
       modalDialog(
          title = "Save Plot As...",
          textInput(ns("save_as_file_name"),
                    "Filename",
                    value = filename),
          shinyWidgets::radioGroupButtons(
             inputId = ns("save_as_file_type"),
             label = "File Type",
             choices = c("jpg", "png", "pdf", "rds"),
             justified = TRUE,
             checkIcon = list(
                yes = tags$i(class = "fa fa-check-square",
                             style = "color: black"),
                no = tags$i(class = "fa fa-square-o",
                            style = "color: black"))
          ),

          conditionalPanel("input.save_as_file_type != 'rds'", ns = ns,
            numericInput(ns("save_width"),
                         "Width of Plot (inches, aspect ratio preserved)",
                         value = "8")
          ),
          footer = tagList(
             modalButton("Cancel"),
             actionButton(ns("save_as_modal_ok"), "OK")
          )
       )
    )
 })



 observeEvent(input$save_as_modal_ok, {

    target_dir <-  file.path(f$project$path, "summary", "figures")
    if(!dir.exists(target_dir)){
       dir.create(target_dir)
    }
    filename <- paste0(input$save_as_file_name, ".", input$save_as_file_type)
    if(input$save_as_file_type == "rds"){
       saveRDS(ee$plot,
               file = file.path(target_dir, filename))
    } else {


      ggplot2::ggsave(filename = file.path(target_dir, filename),
                      plot = ee$plot,
                      height = input$save_width/ee$size_ratio,
                      width = input$save_width,
                      units = "in",
                      bg = "white")
    }
    showNotification(paste("Plot saved as:", filename), type = "message")
    removeModal()
 })




}







    
## To be copied in the UI
# mod_ensemble_average_force_ramp_ui("ensemble_average_force_ramp_1")
    
## To be copied in the server
# mod_ensemble_average_force_ramp_server("ensemble_average_force_ramp_1")
