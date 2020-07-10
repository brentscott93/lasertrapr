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
    
    fluidRow(box(width = 3, collapsible = TRUE, collapsed = FALSE,
                           title = "Auto-Split Observations",
                           #h4(strong("Make Trap Observations")),
                           #strong(h5("1) Select Raw Data Files")),
                           
                           fileInput(ns("trap_txt_upload"),
                                     'Upload Data Files (.txt)',
                                     multiple = TRUE,
                                     accept = ".txt",
                                     buttonLabel = "Browse...",
                                     placeholder = "Data.txt"),
                           
                          
                         # strong( h5("2) Choose number of seconds to split by")),
                           shinyWidgets::knobInput(inputId = ns("threshold"),
                                     label = 'Set Threshold to Split',
                                     value = 20,
                                     min = 10,
                                     max = 30,
                                     inputColor = '#ff41c8',
                                     fgColor = '#ff41c8'),
                           
                           
                           # h5("3) Trap Calibration Files?"),
                           # switchInput(inputId = "trap_cal_files",
                           #             label = NULL,
                           #             value = FALSE,
                           #             onLabel = "Yes",
                           #             offLabel =  "No",
                           #             onStatus = "success",
                           #             offStatus = "danger"),
                           
                           
                         # strong( h5("3) Click button to make observations")),
                           actionButton(inputId = ns("split_obs_button"),
                                        label = "Make Observations",
                                        icon = icon("eye"),
                                        width = "100%", 
                                        style = 'margin-top: 25px;')
                           
      ), #box close,
    
  
            box(title = 'Step Calibration', width = 9,
                fluidRow(column(3, fileInput(ns('step_files'), 
                                             'Upload Step File (.txt)', 
                                             accept = 'text/plain', 
                                             multiple = T,
                                             width = '100%',
                                             placeholder = 'Step.txt'),
                                shinyWidgets::setSliderColor('#ff41c8', 1),
                                   sliderInput(ns('step_cal_stepsize'), 
                                               'Step Cal Step Size', 
                                               min = 1, 
                                               max = 200,
                                               value = 50, 
                                               step = 1, 
                                               ticks = F,
                                               width = '100%'),
                                   actionButton(ns('step_button'), 'Step Cal', width = '100%', style = 'margin-top: 25px;')),
               
                column(9,
                       plotOutput(ns('step'), width = '100%') %>% 
                         shinycssloaders::withSpinner(type = 8, color = "#373B38"))
               )
                
    ) #box close
    ), #rowclose
  
    fluidRow(
            box(title = 'Equipartition', width = 9, height = 325, 
                                fluidRow(
                                  column(3, fileInput(ns('equi_file'), 'Upload Equi File (.txt)', accept = '.txt'),
                                  
                                   withMathJax(helpText("$$\\alpha_{trap}=\\frac{k_B*T_k}{\\sigma^2}$$")),
                                  actionButton(ns('equi_button'), 
                                               'Equi Cal', 
                                               width = '100%', 
                                               placeholder = 'Equi.txt',
                                               style = 'margin-top: 25px;'),
                                ),
                            column(9, 
                                plotOutput(ns('equi'), width = '100%', height = '250px') %>% 
                                  shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                            )
            ),
         
                   
                    valueBoxOutput(ns("step_cal_valueBox"), width = 3),
            valueBoxOutput(ns("equipartition_valueBox"), width = 3)
                          
            )
    
    
     
    ) #tagList
 
  
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
   
    } else if(is_empty(input$trap_txt_upload)){
      showNotification("No data uploaded",
                       type = "error")
    } else {
      req(nchar(f$date$path>0))
      req(input$trap_txt_upload)
      all_data <- map(input$trap_txt_upload$name, ~substring(.x, 1, 4) == 'Data')
      
      if(all(all_data != TRUE)){
        showNotification("Not all files are valid 'Data' files. Only upload files starting with 'Data'.",
                         type = "error")
      } else {
      golem::print_dev('before split_obs call')
      split_obs(input_data = input$trap_txt_upload,
                project = f$project,
                conditions = f$conditions,
                date = f$date,
                threshold = input$threshold)
      f$new_obs_from_split <- f$new_obs_from_split + 1
      }
    }
  })
  
  ####cal####
  #Start trap calibrations
  e <- reactiveValues()
  observeEvent(input$equi_button, {
    if(is_empty(input$equi_file)){
      showNotification('No data uploaded', type = 'error')
    } else if(substring(input$equi_file$name, 1, 4) != 'Equi') {
      showNotification("Not a valid 'Equi' file.", type = 'error')
    } else {
    withProgress(message = "Equipartition Calibration", min= 0, max = 1, value = 0.01, {
      incProgress(0.25, detail = "Reading Data")
      files <- read_tsv(input$equi_file$datapath, col_names = c('bead', 'trap')) %>% 
        dplyr::pull(bead)
      e$vector <- files
      incProgress(0.75, detail = "Calculating")
      
      
      e$cal <-  equipartition(files)
    
    })
    }
  })


  
  output$equi <- renderPlot( {
    req(e$vector)
    plot(e$vector, ylab = 'mV', xlab = 'Datapoints', type = 'l')
  })
  
  output$equipartition_valueBox <-  renderValueBox({
    req(e$cal)
    valueBox(
      value = round(e$cal, 3),
      subtitle = "Trap Stiffness (pN/nm)",
      icon = icon("ruler-vertical"),
      width = "100%",
      color = 'purple'
    )
  })
  
  observeEvent(input$step_button, {
    if(is_empty(input$step_files)){
      showNotification('No data uploaded', type = 'error')
    } else if(substring(input$step_files$name, 1, 4) != 'Step') {
      showNotification("Not a valid 'Step' file.", type = 'error')
    }
  })
    
  
  step_calibration <- eventReactive(input$step_button, {
    req(input$step_files$datapath)
    req(substring(input$step_files$name, 1, 4) == 'Step')
    withProgress(message = "Step Calibration", min= 0, max = 1, value = 0.01, {
      incProgress(0.4, detail = "Reading Data")
      files <- map(input$step_files$datapath, read_tsv, col_names = c('bead', 'trap')) %>%
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
      subtitle = "Step Calibration (nm/mV)",
      icon = icon("ruler-horizontal"),
      width = "100%",
      color = "fuchsia"
    )
  })
  
  step_calibration_plot <- reactive({
    
    grobs <- map(step_calibration(), "plot")
    p <- gridExtra::grid.arrange(grobs = grobs, ncol = 1)
    
    return(p)
    
  })
  
  
  output$step <- renderPlot({
    step_calibration_plot()
  })
}
    
## To be copied in the UI
# mod_split_obs_ui("split_obs")
    
## To be copied in the server
# callModule(mod_split_obs_server, "split_obs")
 
