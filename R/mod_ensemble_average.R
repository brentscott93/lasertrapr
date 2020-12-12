#' ensemble_average UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ensemble_average_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, 
             box(width = NULL,
                 title = "Prep Ensembles",
                 sliderInput(ns("ms_extend_s2"),
                             "Avg of ms to extend forward",
                             value = 3,
                             min = 1, 
                             max = 10, 
                             step = 1, 
                             width = "100%"),
             sliderInput(ns("ms_extend_s1"),
                         "Avg of ms to extend backwards",
                         value = 3,
                         min = 1, 
                         max = 10, 
                         step = 1, 
                         width = "100%"),
             numericInput(ns("hz"), "Hz", value = 5000, min = 0, max = 20000),
             actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%")
             )
      ),
      column(9, 
             box(width = NULL,
                 title = "Average Ensembles",
                 fluidRow(
                   column(5, 
                          sliderInput(ns("forward_signal_ratio"), 
                                      "Forward Signal Ratio",
                                      value = 0, 
                                      min = 0 , 
                                      max = 3, 
                                      step = 0.25)),
                   column(5, 
                          sliderInput(ns("backwards_signal_ratio"), 
                                      "Backwards Signal Ratio",
                                      value = 0, 
                                      min = 0 , 
                                      max = 3, 
                                      step = 0.25)),
                   column(2, actionButton(ns("avg_ensembles"), "Avg Ensembles"))
                 ),
                 fluidRow(plotOutput(ns("forward_backward_ensembles")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                 )
             )
    ),
    fluidRow(
      column(6, 
             box(width = NULL, 
                 title = "Forward Fits")),
      column(6, 
             box(width = NULL, 
                 title = "Backwards Fit"))
    )
  )
}
    
#' ensemble_average Server Function
#'
#' @noRd 
mod_ensemble_average_server <- function(input, output, session, f){
  ns <- session$ns
  observeEvent(input$prep_ensemble, {
    golem::print_dev(f$project_input)
    golem::print_dev(str(f))
    defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
    defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    withProgress(message = "Preparing Ensembles", {
      prep_ensemble(trap_selected_project = f$project$path,
                    ms_extend_s2 = input$ms_extend_s2, 
                    ms_extend_s1 = input$ms_extend_s1, 
                    hz = input$hz)
    })
    showNotification("Ensembles Prepared")
  })
 output$forward_backward_ensembles <- renderPlot({})
 
}
    
## To be copied in the UI
# mod_ensemble_average_ui("ensemble_average")
    
## To be copied in the server
# callModule(mod_ensemble_average_server, "ensemble_average")
 
