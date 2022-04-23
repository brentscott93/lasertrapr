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
             numericInput(ns("hz"), "Hz", value = 5000, min = 0, max = 20000),
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
               title = "Plot"
               
             )
               
               
               ),
               
            box(width = 9,
                 title = "Ensemble Average Plots",
                 fluidRow(
                   column(12, 
                          plotOutput(ns("forward_backward_ensembles")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"))
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
  observeEvent(input$prep_ensemble, {
   # browser()
    golem::print_dev(f$project_input)
    golem::print_dev(str(f))
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    withProgress(message = "Preparing Ensembles", {
      prep_ensemble(trap_selected_project = f$project$path,
                    ms_extend_s2 = input$ms_extend_s2, 
                    ms_extend_s1 = input$ms_extend_s1, 
                    ms_2_skip = input$ms_2_skip,
                    hz = input$hz)
    })
    showNotification("Ensembles Prepared", type= "message")
  })
  
  
   observeEvent(input$avg_ensembles, {
     defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
     defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    
     withProgress(message = "Averaging Ensembles", {
       options_paths <- list.files(path = f$project$path, 
                                   pattern = "options.csv",
                                   full.names = TRUE,
                                   recursive = TRUE)
       
       options_data <-
         data.table::rbindlist(
           lapply(options_paths, data.table::fread),
           fill = TRUE
          )
       
       options_data[include == TRUE & review == TRUE & sucess == TRUE]
       ee$hz <- unique(options_data$hz)
       defend_if(length(ee$hz) != 1, 
                 ui = "Data has different sampling frequencies. Ensmeble averaging not currently supported.", 
                 type = "error")
       
       ee$data <- avg_ensembles(project = f$project_input)
       if(is.null(input$fit) || input$fit == "None"){
          showNotification("Ensembles Averaged", type = "message")
       } else {
         ee$fits <- fit_ensembles(data = ee_data,
                                  fit = input$fit, 
                                  hz = ee$hz)
         showNotification("Ensembles Averaged & Fit", type = "message")
       }
     })
     
   })
   


  
 output$forward_backward_ensembles <- renderPlot({
   req(ee_data())
   ggplot(data = ee_data())+
     geom_point(aes(x = forward_backward_index, 
                    y = avg, 
                    color = conditions), 
                alpha = 0.3,
                shape = 16)+
     facet_wrap(~conditions, scales = "free_x")+
     xlab("Displacement (nm)")+
     theme_cowplot()+
     theme(
       strip.background = element_rect(fill = "transparent"),
       legend.position = "none"
     )
 })
 
}
    
## To be copied in the UI
# mod_ensemble_average_ui("ensemble_average")
    
## To be copied in the server
# callModule(mod_ensemble_average_server, "ensemble_average")
 
