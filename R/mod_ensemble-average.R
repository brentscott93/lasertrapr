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
             actionButton(ns("prep_ensemble"), "Prep Ensembles", width = "100%")
             )
      ),
      column(9, 
             box(width = NULL,
                 title = "Average Ensembles",
                 fluidRow(
                   column(6, sliderInput(ns("length_of_ensembles"), "Ensemble Length (sec)", min = 0.1, max = 2, step = 0.05, value = 1)),
                   column(2, numericInput(ns("hz2"), "Sampling Frequency (hz)", value = 5000)),
                   column(2, actionButton(ns("options"), "Options", width = "100%", style  = "margin-top: 25px")),
                   column(2, actionButton(ns("avg_ensembles"), "Avg Ensembles", width = "100%", style  = "margin-top: 25px"))
                 ),
                 fluidRow(
                   column(12, 
                          plotOutput(ns("forward_backward_ensembles")) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                 )
                 )
             )
    ),
    fluidRow(
      # column(6, 
      #        box(width = NULL, 
      #            title = "Forward Fits")),
      # column(6, 
      #        box(width = NULL, 
      #            title = "Backwards Fit"))
    )
  )
}
    
#' ensemble_average Server Function
#'
#' @noRd 
mod_ensemble_average_server <- function(input, output, session, f){
  ns <- session$ns
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
  
   ee_data <- eventReactive(input$avg_ensembles, {
     defend_if_null(f$project_input, ui = 'Please Select a Project', type = 'error')
     defend_if_blank(f$project_input, ui = "Please Select a Project", type = "error")
    
     withProgress(message = "Averaging Ensembles", {
       ee_data <- avg_ensembles(f = f)
       if(is.null(input$fit) || input$fit == "none"){
          showNotification("Ensembles Averaged", type = "message")
          ee_data
       } else {
         ee_fits <- fit_ensembles(data = ee_data,
                                  fit = input$fit, 
                                  start_list = start_list, 
                                  hz = input$hz)
         showNotification("Ensembles Averaged", type = "message")
         ee_fits
       }
     })
     
   })
   
   start_list <- reactiveValues(
     d1 = 4, 
     d2 = 2, 
     k0 = 250, 
     k1 = 50,
     k2 = 10
   )
   
   observeEvent(input$set_options, {
     start_list$d1 <- input$d1
     start_list$d2 <- input$d2
     start_list$k0 <- input$k0
     start_list$k1 <- input$k1
     start_list$k2 <- input$k2
   })
   observeEvent(input$options,{
     showModal(
       modalDialog(
         title = "Options",
         footer = tagList(modalButton("Cancel"), actionButton(ns("set_options"), "OK")),
         size = "s",
         radioButtons(ns("fit"), 
                      "Fit?",
                      choices = c("none", "1-exp", "2-exp"),
                      selected = "none",
                      inline = TRUE),
         h4(id=ns("starting_title"), "Enter Starting Values:"),
        
         numericInput(ns("d1"), "d1", value = start_list$d1 ),
         numericInput(ns("d2"), "d2", value = start_list$d2),
         
      
         numericInput(ns("k0"), "k0", value = start_list$k0),
         numericInput(ns("k1"), "k1", value = start_list$k1),
         numericInput(ns("k2"), "k2", value = start_list$k2),
        
       )
     )
   })
   observe({
     if(is.null(input$fit) || input$fit == "none"){
       shinyjs::hide("d1")
       shinyjs::hide("d2")
       shinyjs::hide("k0")
       shinyjs::hide("k1")
       shinyjs::hide("k2")
       shinyjs::hide("starting_title")
     } else if(input$fit == "1-exp") {
       shinyjs::show("d1")
       shinyjs::show("d2")
       shinyjs::hide("k0")
       shinyjs::show("k1")
       shinyjs::show("k2")
       shinyjs::show("starting_title")
       } else  {
         shinyjs::show("d1")
         shinyjs::show("d2")
         shinyjs::show("k0")
         shinyjs::show("k1")
         shinyjs::show("k2")
         shinyjs::show("starting_title")
       }
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
 
