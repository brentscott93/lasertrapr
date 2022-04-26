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
             actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%", icon = icon("align-justify"))
             ),
             tabPanel(
               title = "Average",
               sliderInput(ns("length_of_ensembles"), 
                           "Ensemble Length (seconds)", 
                           min = 0.1, 
                           max = 2, 
                           step = 0.05, 
                           value = 1),
               
               shinyWidgets::radioGroupButtons(
                 inputId = ns('fit'),
                 label = "Fit Exponential?",
                 choices = c("None", "1exp", "2exp"),
                 justified = TRUE,
                 checkIcon = list(
                   yes = tags$i(class = "fa fa-check-square",
                                style = "color: black"),
                   no = tags$i(class = "fa fa-square-o",
                               style = "color: black"))
               ),
               
               actionButton(ns("avg_ensembles"), "Avg Ensembles", width = "100%", style  = "margin-top: 25px", icon = icon("calculator"))
               ),
             tabPanel(
               title = "Plot",
               div(style = "diplay: inline-block;", 
                   uiOutput(ns("ee_ui"))),
               uiOutput(ns("facet_col")),
               shinyWidgets::materialSwitch(ns("custom_labels"), "Custom Labels", status = "primary"),
               uiOutput(ns("ee_labels")),
               sliderInput(ns("backwards_shift"), 
                           "Backwards Shift (seconds)",
                           min = 0, 
                           max = 1,
                           step = 0.05,
                           value = 0),
               actionButton(ns("save_plot"), "Save Plot")
               
             )
               
               
               ),
               
            box(width = 8,
                 title = "Ensemble Average Plots",
                 fluidRow(
                   column(12, 
                          plotOutput(ns("forward_backward_ensembles")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                 )
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
    
#' ensemble_average Server Function
#'
#' @noRd 
mod_ensemble_average_server <- function(input, output, session, f){
  ns <- session$ns
  ee <- reactiveValues()
  
  observe({
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
     
     options_data[include == TRUE & review == TRUE & report == "success"]
     ee$hz <- unique(options_data$hz)
    
  })
  
  observeEvent(input$prep_ensemble, {
   # browser()
    
    golem::print_dev(f$project_input)
    golem::print_dev(str(f))
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    defend_if(length(ee$hz) != 1, 
              ui = "Data has different sampling frequencies. Ensmeble averaging not currently supported.", 
              type = "error")
    
    withProgress(message = "Preparing Ensembles", {
      prep_ensemble(trap_selected_project = f$project$path,
                    ms_extend_s2 = input$ms_extend_s2, 
                    ms_extend_s1 = input$ms_extend_s1, 
                    ms_2_skip = input$ms_2_skip,
                    hz = ee$hz)
    })
    showNotification("Ensembles Prepared", type= "message")
  })
  
  
   observeEvent(input$avg_ensembles, {
     defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
     defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
     defend_if(length(ee$hz) != 1, 
               ui = "Data has different sampling frequencies. Ensmeble averaging not currently supported.", 
               type = "error")
    
     withProgress(message = "Averaging Ensembles", {
       ee$data <- avg_ensembles(project = f$project_input)
       if(is.null(input$fit) || input$fit == "None"){
          showNotification("Ensembles Averaged", type = "message")
       } else {
         ee$fits <- fit_ensembles(data = ee$data,
                                  fit = input$fit, 
                                  hz = ee$hz)
         
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
         
         sink(file.path(summary_folder, paste0(Sys.Date(), "_ensemble-average-fits.txt")))
         writeLines("\n
██╗      █████╗ ███████╗███████╗██████╗ ████████╗██████╗  █████╗ ██████╗ ██████╗ 
██║     ██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗██╔══██╗██╔══██╗██╔══██╗
██║     ███████║███████╗█████╗  ██████╔╝   ██║   ██████╔╝███████║██████╔╝██████╔╝
██║     ██╔══██║╚════██║██╔══╝  ██╔══██╗   ██║   ██╔══██╗██╔══██║██╔═══╝ ██╔══██╗
███████╗██║  ██║███████║███████╗██║  ██║   ██║   ██║  ██║██║  ██║██║     ██║  ██║
╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝
\n")
         writeLines("############################### \n Forward Ensemble Average Fits \n ############################### \n")
         print(forward_fits)
         writeLines("\n #### Tidy Table #### \n")
         print(forward_table)
         writeLines("\n ############################### \n Backwards Ensemble Average Fits \n ############################### \n")
         print(backwards_fits)
         writeLines("\n #### Tidy Table #### \n")
         print(backwards_table)
         sink()
         
         
         ee$forward_predict_df <- ee$fits$forward[, predict_forward[[1]], by = conditions]
         
         forward_length <- ee$fits$forward[, .(conditions, total_time_dp)]
         
         
         bwards <- merge(ee$fits$backwards, forward_length, by = "conditions")
         bwards[, predict_backwards_shift := map2(predict_backwards, 
                                                  total_time_dp,
                                                  ~mutate(.x, time_shifted = time+ (2*(.y/ee$hz)))) ]
         
         
         ee$backwards_predict_df <- bwards[, predict_backwards_shift[[1]] , by = conditions]
         
         
         # ee$forward <- ee$data[direction == "forward"]
         # ee$backwards <- ee$data[direction == "backwards"]
       
         showNotification("Ensembles Averaged & Fit", type = "message")
       }
     })
     
   })
   

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
   
   output$facet_col <- renderUI({
     req(conditions())
     if(length(conditions()) >= 2){
     sliderInput(ns("ncol"), 
                 "Number of Facet Columns",
                 min = 1, 
                 max = length(conditions()),
                 step = 1,
                 value = 2)
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
                     ~div(style = 'display:inline-block; ', colourpicker::colourInput(ns(paste0('color', .x)),
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
   
   facet_ncol <- if(length(conditions()) >=2 ){
     req(input$ncol)
     input$ncol
   } else {
     1
   }
   
   plot_colors <- purrr::map_chr(seq_along(conditions()), ~input[[paste0('color', .x)]])
   
   if(!is.null(input$factor_order)){
     if(input$custom_labels){
       if(input$factor_order[length(input$factor_order)] != ""){
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
       }
      }
     } else {
       ee_data$conditions <- factor(ee_data$conditions,
                                    levels = input$factor_order)
       
       ee_forward_predict_df$conditions <- factor(ee_forward_predict_df$conditions, 
                                                  levels = input$factor_order)
       
       ee_backwards_predict_df$conditions <- factor(ee_backwards_predict_df$conditions, 
                                                    levels = input$factor_order)
     }
   }

   ee$plot <-
   ggplot()+
     geom_point(data = ee_data[direction=="forward"],
                aes(x = forward_backward_index/ee$hz, 
                    y = avg, 
                    color = conditions), 
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_point(data = ee_data[direction=="backwards"],
                aes(x = (forward_backward_index/ee$hz) - input$backwards_shift, 
                    y = avg, 
                    color = conditions), 
                alpha = 0.3,
                shape = 16,
                size = 0.8)+
     geom_line(data = ee_forward_predict_df,
               aes(x = time, 
                   y = predict_y))+
     geom_line(data = ee_backwards_predict_df,
               aes(x = time_shifted - input$backwards_shift, 
                   y = predict_y))+
     facet_wrap(~conditions, scales = "free_x", ncol = facet_ncol)+
     ylab("nanometers")+
     xlab("seconds")+
     scale_color_manual(values = plot_colors)+
     theme_cowplot()+
     theme(
       strip.background = element_rect(fill = "transparent"),
       legend.position = "none"
     )
 })
 
 output$forward_backward_ensembles <- renderPlot({
   validate(need(ee$data, "Prep & Average Ensembles"))
   validate(need(ee$fits,"Average Ensembles Before Continuing"))
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
         
          conditionalPanel("input.save_as_file_type ! = 'rds'", ns = ns,
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
       saveRDS(fig$gg_final, 
               file = file.path(target_dir, filename))
    } else {
       
       
       ggsave(filename = file.path(target_dir, filename),
              plot = ee$plot,
              height = input$save_width/ee$size_ratio,
              width = input$save_width, 
              units = "in",
              bg = "white")
    }
    showNotification(paste("Plot saved as:", filename))
    removeModal()
 })
 
}
    
## To be copied in the UI
# mod_ensemble_average_ui("ensemble_average")
    
## To be copied in the server
# callModule(mod_ensemble_average_server, "ensemble_average")
 
