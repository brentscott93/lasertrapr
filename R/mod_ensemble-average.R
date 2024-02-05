#' ensemble_average UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @noRd 
#' @importFrom shiny NS tagList 
mod_ensemble_average_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
        tabBox(width = 3,
               tabPanel(
               title = "Prep",
               uiOutput(ns("prep_tab")),
             ##     sliderInput(ns("ms_extend_s2"),
             ##                 "Avg of ms to extend forward",
             ##                 value = 3,
             ##                 min = 1,
             ##                 max = 10,
             ##                 step = 1,
             ##                 width = "100%"),
             ##     sliderInput(ns("ms_2_skip"),
             ##                 "Number of ms to skip before s1 avg",
             ##                 value = 5,
             ##                 min = 1,
             ##                 max = 10,
             ##                 step = 1,
             ##                 width = "100%"),
             ## sliderInput(ns("ms_extend_s1"),
             ##             "Avg of ms to extend s1 backwards",
             ##             value = 3,
             ##             min = 1,
             ##             max = 10,
             ##             step = 1,
             ##             width = "100%"),
             ## actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%", icon = icon("align-justify"))
             ),
             tabPanel(
               title = "Average",
               uiOutput(ns("average_tab")),
               ),
             tabPanel(
               title = "Plot Options",
               div(style = "diplay: inline-block;", 
                   uiOutput(ns("ee_ui"))),
               uiOutput(ns("facet_col")),
               shinyWidgets::materialSwitch(ns("custom_labels"), "Custom Labels", status = "primary"),
               uiOutput(ns("ee_labels")),
              
               sliderInput(ns("backwards_shift"), 
                           "Backwards Shift (seconds)",
                           min = -1,
                           max = 1,
                           step = 0.05,
                           value = 0),
               numericInput(ns("size"), "Theme Size", value = 14),
               actionButton(ns("save_plot"), "Save Plot", width = "100%")
               
             )
               
               
               ),

      uiOutput(ns("main_box")),
               
            ## tabBox(side = "right",
            ##  width = 8,
            ##   title = "Ensemble Averages",
            ##  tabPanel(
            ##     "Substeps",
            ##     tableOutput(ns("substeps")),
            ##    plotOutput(ns("substeps_plot"))
            ##  ),
            ##  tabPanel(
            ##     "Fit - Backwards Pars",
            ##     tableOutput(ns("backwards_par"))
            ##  ),

            ##  tabPanel(
            ##     "Fit - Forward Pars",
            ##     tableOutput(ns("forward_par"))
            ##  ),
            ##  tabPanel(
            ##     "Plot",
            ##     plotOutput(ns("forward_backward_ensembles"), height = "auto") |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
            ##  ),
             
            ## ),
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
    
#' ensemble_average Server Function
#'
#' @noRd
#' @import data.table ggplot2 cowplot
mod_ensemble_average_server <- function(input, output, session, f, input_sidemenu){
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
                 "Substeps",
                 tableOutput(ns("substeps")),
                 plotOutput(ns("substeps_plot"))
               ),
               tabPanel(
                 "Fit - Backwards Pars",
                 tableOutput(ns("backwards_par"))
               ),

               tabPanel(
                 "Fit - Forward Pars",
                 tableOutput(ns("forward_par"))
               ),
               tabPanel(
                 "Plot",
                 plotOutput(ns("forward_backward_ensembles"), height = "auto") |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
               )
               )
      )

    } else if(ee$analyzer == "mini"){
      tagList(
        tabBox(side = "right",
               width = 8,
               title = "Mini Ensemble, Ensemble Averages",
               tabPanel(
                 "Fits",
                 tableOutput(ns("mini_fit"))
               ),
               tabPanel(
                 "Plot",
                 plotOutput(ns("mini_ensemble_plot"), height = "auto") |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
               ),
               )

      )
    } else {
     tagList(h4("Data needs to be analyzed before ensemble averaging."))
    }

  })


  output$prep_tab <- renderUI({
    if(is.null(ee$analyzer)){
   tagList(h4("Data not analyzed"))
    } else if(ee$analyzer == "hm/cp" || ee$analyzer == "covar"){
      tagList(
        sliderInput(ns("ms_extend_s2"),
                    "Avg of ms to extend forward",
                    value = 3,
                    min = 1,
                    max = 10,
                    step = 1,
                    width = "100%"),
        sliderInput(ns("ms_2_skip"),
                    "Number of ms to skip before s1 avg",
                    value = 5,
                    min = 1,
                    max = 10,
                    step = 1,
                    width = "100%"),
        sliderInput(ns("ms_extend_s1"),
                    "Avg of ms to extend s1 backwards",
                    value = 3,
                    min = 1,
                    max = 10,
                    step = 1,
                    width = "100%"),
        numericInput(ns("tmin_ms"), label = "Tmin (ms)", value = 5, step = 1, width = "49%"),
        numericInput(ns("downsample_ensemble_by"), "Downsample ensemble by (factor)", value = 1, step = 1, width = "49%"),
        actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%", icon = icon("align-justify"))
      )
    } else if(ee$analyzer == "mini"){
      tagList(
        actionButton(ns("prep_ensemble"), "Prep Mini Ensembles", width = "100%", icon = icon("align-justify"))
      )
    }

  })

  output$average_tab <- renderUI({
    if(is.null(ee$analyzer) || ee$analyzer == "hm/cp" || ee$analyzer == "covar"){
      tagList(
       fluidRow(column(6,
        numericInput(ns("length_of_ensembles_forward"),
                    "Forward Length (s)",
                    min = 0.1,
                    value = 1)
        ),
        column(6,

        numericInput(ns("length_of_ensembles_backwards"),
                    "Backwards Length (seconds)",
                    min = 0.1,
                    value = 1)
        )
        ),

        shinyWidgets::radioGroupButtons(
                        inputId = ns('fit'),
                        label = "Fit Exponential?",
                        choices = c("1exp", "2exp", "3exp"),
                        justified = TRUE,
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-check-square",
                                       style = "color: black"),
                          no = tags$i(class = "fa fa-square-o",
                                      style = "color: black"))
                      ),

        conditionalPanel("input.fit == '1exp'", ns=ns,
                         withMathJax(helpText("$$ y = d_1 + d_2(1-e^{-k_1x}) $$"))),

        conditionalPanel("input.fit == '2exp'", ns=ns,
                         withMathJax(helpText("$$ y = d_1(1-e^{-k_0x}) + d_2(1-e^{-k_1x}) $$"))),

        conditionalPanel("input.fit == '3exp'", ns=ns,
                         withMathJax(helpText("$$ y = d_1(1-e^{-k_1x}) + d_2(1-e^{-k_2x}) + d_3(1-e^{-k_3x}) $$"))),

        actionButton(ns("avg_ensembles"), "Avg Ensembles", width = "49%", style  = "margin-top: 25px", icon = icon("calculator")),
        actionButton(ns("fit_ensembles"), "Avg Ensembles", width = "49%", style  = "margin-top: 25px", icon = icon("square-root-variable"))
      )

    } else if(ee$analyzer == "mini"){
      tagList(
        actionButton(ns("avg_ensembles"), "Avg Ensembles", width = "100%", style  = "margin-top: 25px", icon = icon("calculator"))
      )
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

      if(analyzer == "hm/cp" || analyzer == "covar"){
      prep_ensemble(trap_selected_project = f$project$path,
                    ms_extend_s2 = input$ms_extend_s2, 
                    ms_extend_s1 = input$ms_extend_s1, 
                    ms_2_skip = input$ms_2_skip,
                    hz = ee$hz,
                    tmin_ms = input$tmin_ms,
                    analyzer = analyzer ,
                    downsample_ensemble_by = input$downsample_ensemble_by)
      } else if(analyzer == "mini"){
        align_mini_events(project = f$project$name, is_shiny = TRUE)
      } else {
        showNotification("Whoops. Data analyzed with an analyzer that does not support ensemble averaging.")
      }
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
       if(analyzer == "hm/cp" || analyzer == "covar"){
       print(f$project_path)
       avg_ensembles(project = f$project_input)
       showNotification("Ensembles Averaged", type = "message")
} else if(analyzer == "mini"){

  ee$mini_ensemble_ensembles <- avg_aligned_mini_events(project = f$project_input, is_shiny = TRUE)



         summary_folder <- file.path(f$project$path, "summary")
         if(!file.exists(summary_folder)){
           dir.create(summary_folder)
         }

         mini_fits <- ee$mini_ensemble_ensembles$nls_models

         sink(file.path(summary_folder, paste0(Sys.Date(), "_mini-ensemble-ensemble-average-models.txt")))
         writeLines("\n
██╗      █████╗ ███████╗███████╗██████╗ ████████╗██████╗  █████╗ ██████╗ ██████╗
██║     ██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗██╔══██╗██╔══██╗██╔══██╗
██║     ███████║███████╗█████╗  ██████╔╝   ██║   ██████╔╝███████║██████╔╝██████╔╝
██║     ██╔══██║╚════██║██╔══╝  ██╔══██╗   ██║   ██╔══██╗██╔══██║██╔═══╝ ██╔══██╗
███████╗██║  ██║███████║███████╗██║  ██║   ██║   ██║  ██║██║  ██║██║     ██║  ██║
╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝
\n")
         writeLines("############################### \n Mini Ensemble, Ensemble Average Fits \n ############################### \n")
         print(lapply(mini_fits, base::summary))
         sink()

         showNotification("Mini Ensembles Averaged & Fit", type = "message")

}

})

     })

   observeEvent(input$fit_ensembles, {
     defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
     defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
## browser()
       ea_file <- file.path(path.expand("~"), "lasertrapr", f$project_input, "summary", "ensemble-averages.csv")

    defend_if(!file.exists(ea_file),
              ui = "Ensemble-averages.csv does not exists",
              type = "error")

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
       ee$data <- fread(ea_file)

       if(analyzer == "hm/cp" || analyzer == "covar"){
       if(is.null(input$fit) || input$fit == "None"){
          showNotification("Ensembles Averaged", type = "message")
       } else {
         ee$fits <- fit_ensembles(data = ee$data,
                                  fit = input$fit, 
                                  hz = ee$hz,
                                  max_l_forward = input$length_of_ensembles_forward,
                                  max_l_backwards = input$length_of_ensembles_backwards)
         
         summary_folder <- file.path(f$project$path, "summary")
         if(!file.exists(summary_folder)){
           dir.create(summary_folder)
         }
         
         forward_fits <- ee$fits$forward$forward_fit
         names(forward_fits) <- ee$fits$forward$conditions
         forward_table <- ee$fits$forward$forward_fit_par_table
         names(forward_table) <- ee$fits$forward$conditions
         
         backwards_fits <- ee$fits$backwards$backwards_fit
         names(backwards_fits) <- ee$fits$backwards$conditions
         backwards_table <- ee$fits$backwards$backwards_fit_par_table
         names(backwards_table) <- ee$fits$backwards$conditions
         
         sink(file.path(summary_folder, paste0(Sys.Date(), "_ensemble-average-models.txt")))
         writeLines("\n
██╗      █████╗ ███████╗███████╗██████╗ ████████╗██████╗  █████╗ ██████╗ ██████╗ 
██║     ██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗██╔══██╗██╔══██╗██╔══██╗
██║     ███████║███████╗█████╗  ██████╔╝   ██║   ██████╔╝███████║██████╔╝██████╔╝
██║     ██╔══██║╚════██║██╔══╝  ██╔══██╗   ██║   ██╔══██╗██╔══██║██╔═══╝ ██╔══██╗
███████╗██║  ██║███████║███████╗██║  ██║   ██║   ██║  ██║██║  ██║██║     ██║  ██║
╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝
\n")
         writeLines("############################### \n Forward Ensemble Average Fits \n ############################### \n")
         print(lapply(forward_fits, base::summary))
         # writeLines("\n #### Tidy Table #### \n")
         # print(forward_table)
         writeLines("\n ############################### \n Backwards Ensemble Average Fits \n ############################### \n")
         print(lapply(backwards_fits, base::summary))
         # writeLines("\n #### Tidy Table #### \n")
         # print(backwards_table)
         sink()
         
         
         ee$forward_predict_df <- ee$fits$forward[, predict_forward[[1]], by = conditions]
         ee$forward_fitted_raw_df <- ee$fits$forward[, ensemble_k1_prep[[1]], by = conditions]

         ee$avg_tail <- ee$fits$forward[, .(conditions, avg_tail)]

         forward_length <- ee$fits$forward[, .(conditions, total_time_dp)]



         bwards <- merge(ee$fits$backwards, forward_length, by = "conditions")
         bwards[, predict_backwards_shift := purrr::map2(predict_backwards,
                                                  total_time_dp,
                                                  ~dplyr::mutate(.x, time_shifted = time+(2*(.y/ee$hz)))) ]



         bwards[, raw_backwards_shift := purrr::map2(ensemble_k2_prep,
                                                  total_time_dp,
                                                  ~dplyr::mutate(.x, time_shifted = time+(2*(.y/ee$hz)))) ]

         bwards[, raw_backwards_baseline_shift := purrr::map2(backwards_baseline_shifted,
                                                  total_time_dp,
                                                  ~dplyr::mutate(.x, time_shifted = time+(2*(.y/ee$hz)))) ]


         ee$backwards_predict_df <- bwards[, predict_backwards_shift[[1]] , by = conditions]
         ee$backwards_fitted_raw_df <- bwards[, raw_backwards_shift[[1]] , by = conditions]
         ee$backwards_baseline_df <- bwards[, raw_backwards_baseline_shift[[1]] , by = conditions]

         ee$avg_head <- bwards[, .(conditions, avg_head)]

         ee$fdat <- ee$fits$forward[, forward_fit_par_table[[1]], by=conditions]
         forward_pars <- ee$fdat
         forward_pars$direction <- "forward"
         
         ee$bdat <- ee$fits$backwards[, backwards_fit_par_table[[1]], by=conditions]
         backwards_pars <- ee$bdat
         backwards_pars$direction <- "backwards"
         
         mod_table <- rbind(forward_pars, backwards_pars)
         data.table::fwrite(mod_table, file = file.path(summary_folder, paste0(Sys.Date(), "_ensemble-average-parameters.csv")))
         # ee$forward <- ee$data[direction == "forward"]
         # ee$backwards <- ee$data[direction == "backwards"]

         substeps_path <-
           list.files(f$project$path,
                      pattern = "substeps.csv",
                      recursive = TRUE,
                      full.names = TRUE)

         substeps_data <-
           rbindlist(
             lapply(
               substeps_path,
               fread
             )
           )

          ee$substeps_data <- substeps_data
          data.table::fwrite(substeps_data, file = file.path(summary_folder, paste0(Sys.Date(), "_all-substeps-ensemble-average.csv")))

         showNotification("Ensembles Averaged & Fit", type = "message")
       }

       } else {


         showNotification("Unsupported data type", type = "message")
       }


     })
     
   })
   

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
   req(ee$forward_predict_df)
   req(input$color1)
   
   ee_data <- ee$data
   ee_forward_predict_df <- ee$forward_predict_df
   ee_backwards_predict_df <- ee$backwards_predict_df

   ee_forward_fitted_raw_df <- ee$forward_fitted_raw_df
   ee_backwards_fitted_raw_df <- ee$backwards_fitted_raw_df

   ee_backwards_baseline_df <- ee$backwards_baseline_df

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
       
       ee_forward_predict_df$conditions <- factor(ee_forward_predict_df$conditions, 
                                                  levels = input$factor_order, 
                                                  labels = new_labels)
       
       ee_backwards_predict_df$conditions <- factor(ee_backwards_predict_df$conditions, 
                                                  levels = input$factor_order, 
                                                  labels = new_labels)



       ee_backwards_baseline_df$conditions <- factor(ee_backwards_baseline_df$conditions,
                                                  levels = input$factor_order,
                                                  labels = new_labels)

       ee_forward_fitted_raw_df$conditions <- factor(ee_forward_fitted_raw_df$conditions,
                                                  levels = input$factor_order,
                                                  labels = new_labels)


       ee_backwards_fitted_raw_df$conditions <- factor(ee_backwards_fitted_raw_df$conditions,
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
       
       ee_forward_predict_df$conditions <- factor(ee_forward_predict_df$conditions, 
                                                  levels = input$factor_order)
       
       ee_backwards_predict_df$conditions <- factor(ee_backwards_predict_df$conditions, 
                                                    levels = input$factor_order)


       ee_backwards_baseline_df$conditions <- factor(ee_backwards_baseline_df$conditions,
                                                  levels = input$factor_order)

       ee_forward_fitted_raw_df$conditions <- factor(ee_forward_fitted_raw_df$conditions,
                                                  levels = input$factor_order)

       ee_backwards_fitted_raw_df$conditions <- factor(ee_backwards_fitted_raw_df$conditions,
                                                  levels = input$factor_order)

     }
       }
       }
   }

   length_of_full_ensemble <- nrow(ee_data[direction=="forward" & ensemble_index >= 0])
   diff_in_length_filter <- length_of_full_ensemble - nrow(ee_forward_predict_df)
   hz <- ee$hz
   shift <- input$backwards_shift

   original_length <- nrow(ee_data[direction == "forward" & ensemble_index >= 0])
   max_forward_fit_index <- nrow(ee_forward_predict_df)
   ee_data_forward_baseline <- ee_data[direction=="forward" & forward_backward_index < 0 ]

   ee_data_backwards_baseline <- ee_data[direction=="backwards" & ensemble_index > 0]
   ## ee_data_backwards_baseline[, time_shifted := time+2]

   ## if(max_forward_fit_index < original_length){
   ##   filter_diff <- original_length-max_forward_fit_index
   ##   ee_data_backwards[, forward_backward_index := forward_backward_index-(2*filter_diff)]
   ##   }



   ee$plot <-
   ggplot()+
     geom_point(data = ee_data_forward_baseline,
                aes(x = ensemble_index/hz,
                    y = avg,
                    color = conditions),
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_point(data = ee_backwards_baseline_df,
                aes(x = time_shifted - shift,
                    y = avg,
                    color = conditions),
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_point(data = ee_forward_fitted_raw_df,
                aes(x = time,
                    y = avg, 
                    color = conditions), 
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_point(data = ee_backwards_fitted_raw_df,
                aes(x = time_shifted - shift,
                    y = avg, 
                    color = conditions), 
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_line(data = ee_forward_predict_df,
               aes(x = time,
                   y = predict_y))+
     geom_line(data = ee_backwards_predict_df,
               aes(x = time_shifted - shift,
                   y = predict_y))+
     facet_wrap(~conditions, scales = "free_x", nrow = facet_nrow)+
     ylab("nanometers")+
     xlab("seconds")+
     scale_color_manual(values = plot_colors)+
     cowplot::theme_cowplot(input$size)+
     theme(
       strip.background = element_rect(fill = "transparent"),
       legend.position = "none"
     )
 })
 
 output$forward_backward_ensembles <- renderPlot({
   validate(need(ee$data, "Prep & Average Ensembles"))
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
 
 
 output$forward_par <- renderTable({
    validate(need(ee$fdat, "Please average & fit ensembles"))
    dcast(ee$fdat, conditions ~ term, value.var = "estimate")
 })
 
 output$backwards_par <- renderTable({
    validate(need(ee$bdat, "Please average & fit ensembles"))
    dcast(ee$bdat, conditions ~ term, value.var = "estimate")

 })

  output$substeps <- renderTable({
    validate(need(ee$substeps_data, "Please average & fit ensembles"))

    tab <- ee$substeps_data[, .(total_step = mean(bead_position_substep_2_nm - prior_unbound_position_nm),
                                substep_1 = mean(substep_1_nm, na.rm = TRUE),
                                substep_2  = mean(substep_2_nm, na.rm = TRUE)),
                            by = "conditions"]

    tab[, .(Conditions = conditions,
            "Total Step" = total_step,
            "Substep 1" = substep_1 ,
            "Substep 2" = substep_2)]

    ## tab <- merge(ee$avg_tail, ee$avg_head, by = "conditions")
    ## tab[, .(Conditions = conditions,
    ##         "Total Step" = avg_tail,
    ##         "Substep 1" = avg_head,
    ##         "Substep 2" = avg_tail - avg_head)]
    })

  output$substeps_plot <- renderPlot({
    req(ee$substeps_data)

    substeps_data <- ee$substeps_data

    substeps_data[, total_step := bead_position_substep_2_nm - prior_unbound_position_nm]

    if(is.null(ee$plot_colors)){
      plot_colors <- unname(palette.colors())
    } else {
      plot_colors <- ee$plot_colors
    }

    total <- plot_ecdf(measured_events = substeps_data,
                      var = "total_step",
                      colorz = plot_colors,
                      x_lab = "nanometers",
                      title = "Total Displacement",
                      basesize = 12)

    sub1 <- plot_ecdf(measured_events = substeps_data,
                      var = "substep_1_nm",
                      colorz = plot_colors,
                      x_lab = "nanometers",
                      title = "Substep 1",
                      basesize = 12)


    sub2 <- plot_ecdf(measured_events = substeps_data,
                      var = "substep_2_nm",
                      colorz = plot_colors,
                      x_lab = "Nanometers",
                      title = "Substep 2",
                      basesize = 12)

    cowplot::plot_grid(total, sub1, sub2, nrow = 1)
  })


}
## To be copied in the UI
# mod_ensemble_average_ui("ensemble_average")
    
## To be copied in the server
# callModule(mod_ensemble_average_server, "ensemble_average")
 
