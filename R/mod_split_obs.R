#' split_obs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_split_obs_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(column(3, box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                           title = "Auto-Split Observations",
                           #h4(strong("Make Trap Observations")),
                           h5("1) Select Raw Data Files"),
                           
                           fileInput(ns("trap_txt_upload"),
                                     NULL,
                                     multiple = TRUE,
                                     accept = ".txt",
                                     buttonLabel = "Browse...",
                                     placeholder = "Select .txt"),
                           
                           br(),
                          
                           h5("2) Choose number of seconds to divide obs by"),
                           shinyWidgets::knobInput(inputId = ns("threshold"),
                                     label = NULL,
                                     value = 20,
                                     min = 10,
                                     max = 30),
                           
                           
                           # h5("3) Trap Calibration Files?"),
                           # switchInput(inputId = "trap_cal_files",
                           #             label = NULL,
                           #             value = FALSE,
                           #             onLabel = "Yes",
                           #             offLabel =  "No",
                           #             onStatus = "success",
                           #             offStatus = "danger"),
                           
                           
                           h5("3) Click button to make observations"),
                           actionButton(inputId = ns("split_obs_button"),
                                        label = "Make Observations",
                                        icon = icon("eye"),
                                        width = "100%")
                           
      ) #box close,
     ),#col close
     column(6, 
            fluidRow(column(12, 
                            box(title = 'Equipartition',
                                fileInput('equi_file', 'Upload Equi File (.txt)', accept = '.txt'),
                                actionButton(ns('equi_button'), 'Equi Cal'),
                                plotOutput(ns('equi')) %>% 
                                  shinycssloaders::withSpinner(type = 8, color = "#373B38")
                            ))),
            fluidRow(column(12, 
                            box(title = 'Step Calibration',
                                fileInput('step_file', 'Upload Step File (.txt)', accept = '.txt'),
                                actionButton(ns('step_button'), 'Equi Cal'),
                                plotOutput(ns('step')) %>% 
                                  shinycssloaders::withSpinner(type = 8, color = "#373B38"))))
            ),
            column(3, 
                   valueBoxOutput("equipartition_valueBox", width = NULL),
                    valueBoxOutput("step_cal_valueBox", width = NULL)
                          
            )
    ) #row close
 
  )
}
    
#' split_obs Server Function
#'
#' @noRd 
mod_split_obs_server <- function(input, output, session, f){
  ns <- session$ns
 
  #check if a date folder is properly selected
  observeEvent(input$split_obs_button, {
    if(is_empty(f$date) == TRUE){
      showNotification("No 'Date' folder selected. Please select a folder with the folder chooser above.",
                       type = "error")
   
    } else {
      
      req(nchar(f$date$path>0))
      req(input$trap_txt_upload)
      golem::print_dev('before split_obs call')
      split_obs(input_data = input$trap_txt_upload,
                project = f$project,
                conditions = f$conditions,
                date = f$date,
                threshold = input$threshold)
    }
  })
  
  ####cal####
  #Start trap calibrations
  
  
  equi_cal <- eventReactive(input$equi_button, {
    req(input$equi_file)
    withProgress(message = "Equipartition Calibration", min= 0, max = 1, value = 0.01, {
      incProgress(0.25, detail = "Reading Data")
      files <- read_tsv(input$equi_file$datapath, col_names = c('bead', 'trap')) %>% 
        dplyr::pull(bead)
      
      incProgress(0.75, detail = "Calculating")
    
      
      cal1 <-  equipartition(files)
      
    })
    return(cal1)
    
  })
  
  output$equipartition_valueBox <-  renderValueBox({
    
    valueBox(
      value = round(equi_cal(), 3),
      subtitle = "pN/nm",
      icon = icon("ruler-vertical"),
      width = "100%",
      color = "fuchsia"
    )
  })
  
  
  step_calibration <- eventReactive(input$trap_cal_actionButton, {
    req(input$trap_cal_files == TRUE)
    withProgress(message = "Step Calibration", min= 0, max = 1, value = 0.01, {
      step_files <- list_files(path = paste0(trap_selected_date()$path, "/cal")) %>%
        filter(str_detect(name, "Step"))
      incProgress(0.4, detail = "Reading Data")
      files <- map(step_files$path, read_csv) %>%
        map(pull, bead)
      incProgress(0.75, detail = "Calculating...This may take a while...")
      steps <- map(files, step_cal, step = input$step_cal_stepsize)
      incProgress(1, detail = "Done!")
    })
    return(steps)
  })
  
  conversion <- reactive({
    conv1 <- map(step_calibration(), "mv2nm_conversion")
    conv2 <-  round(mean(abs(unlist(conv1))), 2)
  })
  
  output$step_cal_valueBox <-  renderValueBox({
    valueBox(
      value = conversion(),
      subtitle = "nm/mV",
      icon = icon("ruler-horizontal"),
      width = "100%",
      color = "lime"
    )
  })
  
  step_calibration_plot <- reactive({
    
    grobs <- map(step_calibration(), "plot")
    p <- grid.arrange(grobs = grobs, ncol = 2)
    
    return(p)
    
  })
  
  
  output$step_cal_plot <- renderPlot({
    step_calibration_plot()
  })
}
    
## To be copied in the UI
# mod_split_obs_ui("split_obs")
    
## To be copied in the server
# callModule(mod_split_obs_server, "split_obs")
 
