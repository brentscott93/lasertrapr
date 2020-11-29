#' split_obs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import tidyverse magrittr
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
                                     max = 30),
                                     # inputColor = '#ff41c8',
                                     # fgColor = '#ff41c8'),
                           
                           
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
                                #shinyWidgets::setSliderColor('#ff41c8', c(1, 2)),
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
                                  column(3, fileInput(ns('equi_file'), 
                                                      'Upload Equi File (.txt)',
                                                      placeholder = 'Equi.txt',
                                                      accept = '.txt'),
                                  
                                   withMathJax(helpText("$$\\alpha_{trap}=\\frac{k_B*T_k}{\\sigma^2}$$")),
                                  actionButton(ns('equi_button'), 
                                               'Equi Cal', 
                                               width = '100%', 
                                               style = 'margin-top: 25px;'),
                                ),
                            column(9, 
                                plotOutput(ns('equi'), width = '100%', height = '250px') %>% 
                                  shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                            )
            ),
         
            tags$style(".small-box.bg-yellow { background-color: #1B9E77 !important; color: #f2f2f2 !important; }"), 
            valueBoxOutput(ns("step_cal_valueBox"), width = 3),
            valueBoxOutput(ns("equipartition_valueBox"), width = 3)
                          
            ), #row close
    fluidRow(
      box(title = "Simulate Data", width = 12, collapsible = T, collapsed = T,
          fluidRow(
            column(2,
             numericInput(ns("sim_n_events"), "Events", value = 100, min = 10, max = 200, step = 1)
            ),
            column(2,
                   numericInput(ns("sim_signal"), "Signal", value = 2.5, min = 1.5, max = 10, step = 0.1)
            ),
            column(2,
                   numericInput(ns("sim_hz"), "Hz", value = 5000, min = 0, max = 20000)
            ),
            column(2,
                    actionButton(ns("sim_options"), "Options", width = "100%", style = "margin-top: 25px;")
            ),
            column(2,
                   actionButton(ns("sim_go"), "Simulate Data", width = "100%", style = "margin-top: 25px;")
            ),
            column(2,
                   actionButton(ns("sim_save"), "Save", width = "100%", style = "margin-top: 25px;")
            )
          ),
          fluidRow(
            column(3, 
                   h6("Current Simulation Parameters:"), 
                   verbatimTextOutput(ns("sim_parameters"))
            ),
            column(9, 
                   dygraphs::dygraphOutput(ns('sim')) %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
          )
        )
      )
   )
  ) #tagList
 
  
}
    
#' split_obs Server Function
#'
#' @noRd 
#' @import tidyverse magrittr
mod_split_obs_server <- function(input, output, session, f){
  ns <- session$ns
 
  #check if a date folder is properly selected
  observeEvent(input$split_obs_button, {
    golem::print_dev("go")
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
      color = 'yellow'
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
      color = "yellow"
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
  
  
  observeEvent(input$sim_options, {
    showModal(
      modalDialog(
        size = "l",
       title = "Define Simulation Parameters",
       footer = modalButton("OK"),
       tabsetPanel(
         tabPanel("Baseline",
          sliderInput(ns("sim_baseline_mean"), "Mean", value = 0, step = 1, round = TRUE, min = -50, max = 50, width = "100%"),
          sliderInput(ns("sim_baseline_sd"), "SD", value = 8, step = 1, round = TRUE, min = 0, max = 15,  width = "100%"),
          plotOutput(ns("sim_baseline_histogram"))
          ),
          tabPanel("Displacements",
           sliderInput(ns("sim_displacement_mean"), "Mean", value = 5, step = 1, round = TRUE, min = -50, max = 50, width = "100%"),
           sliderInput(ns("sim_displacement_sd"), "SD", value = 8, step = 1, round = TRUE, min = 0, max = 15, width = "100%"),
           plotOutput(ns("sim_displacement_histogram"))
          ),
         tabPanel("Pi Release",
           br(),
           shinyWidgets::radioGroupButtons(
             inputId = ns('sim_pi_release_occurs'),
             label = "Pi release before or after stroke?",
             choices = c("Before" = "before",
                         "After" = "after"),
             justified = TRUE,
             selected = 'after',
             checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square",
                              style = "color: black"),
                              no = tags$i(class = "fa fa-square-o",
                              style = "color: black"))
                  ),
            fluidRow(
              column(6,
               numericInput(ns("sim_pi_release_rate"), "Avg Rate (Hz)", value = 200, max = 500, min = 0, step = 5, width = "100%")
              ), 
              column(6, 
                     div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_pi_release_rate_conversion")))
              )
            ),
            sliderInput(ns("sim_pi_release_lower"), "Lower (ms)", value = 1, step = 1, round = TRUE, min = 1, max = 100, width = "100%"),
            sliderInput(ns("sim_pi_release_upper"), "Upper (ms)", value = 5000, step = 25, round = TRUE, min = 100, max = 5000 , width = "100%"),
            plotOutput(ns("sim_pi_release_histogram"))
         ),
          tabPanel("ADP Release",
             fluidRow(
               column(4,
                numericInput(ns("sim_adp_release_rate"), "Avg Rate (Hz)", value = 25, max = 500, min = 0, step = 5, width = "100%"),
               ), 
               column(4, 
                      div(style = 'margin-top: 22px;',  verbatimTextOutput(ns("sim_adp_release_conversion")))
               ),
               column(4, 
                      numericInput(ns("sim_hitch_size"), "Hitch Size (nm)", value = 1.5, step = 0.5,  min = 0, max = 10,  width = "100%")
              )
             ),
           sliderInput(ns("sim_adp_release_lower"), "Lower (ms)", value = 1, step = 5, round = TRUE, min = 1, max = 200, width = "100%"),
           sliderInput(ns("sim_adp_release_upper"), "Upper (ms)", value = 1000, step = 10, round = TRUE, min = 50, max = 1000, width = "100%"),
           
           plotOutput(ns("sim_adp_release_histogram"))
          ),
         tabPanel("ATP Binding",
           fluidRow(
             column(6,
              numericInput(ns("sim_atp_binding_rate"), "Rate (1/mean)", value = 1000, max = 5000, min = 0, step = 10, width = "100%")
             ),
              column(6, 
                     div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_atp_binding_conversion")))
               )
            ),
           sliderInput(ns("sim_atp_binding_lower"), "Lower (ms)", value = 1, step = 10, round = TRUE, min = 1, max = 2000,  width = "100%"),
           sliderInput(ns("sim_atp_binding_upper"), "Upper (ms)", value = 20000, step = 1000, round = TRUE, min = 1000, max = 20000, width = "100%"),
           plotOutput(ns("sim_atp_binding_histogram"))
          ),
         
         tabPanel("Time Off",
          fluidRow(
           column(6,
            numericInput(ns("sim_time_off_rate"), "Rate (1/mean)", value = 50, max = 200, min = 0, step = 5, width = "100%")
           ),
           column(6, 
                  div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_time_off_conversion")))
           )
          ),
          sliderInput(ns("sim_time_off_lower"), "Lower (ms)", value = 1, step = 5, round = TRUE, min = 1, max = 100, width = "100%"),
          sliderInput(ns("sim_time_off_upper"), "Upper (ms)", value = 1000, step = 5, round = TRUE, min = 100, max = 1000, width = "100%"),
          plotOutput(ns("sim_time_off_histogram"))
         )
        )
       )
      )
  })
  

  output$sim_baseline_histogram <- renderPlot({
    x <- rnorm(100000, input$sim_baseline_mean, input$sim_baseline_sd)
    hist(x, xlab = "nm", main = "Simulated Baseline Distribution (n = 100k)", breaks = (min(x)-1):(max(x)+1), freq = F)
    curve(dnorm(x, input$sim_baseline_mean, input$sim_baseline_sd), add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_displacement_histogram <- renderPlot({
    x <- rnorm(100000, mean = input$sim_displacement_mean, sd = input$sim_displacement_sd)
    hist(x, xlab = "nm", main = "Simulated Displacement Distribution (n = 100k)", breaks = (min(x)-1):(max(x)+1), freq = F)
    curve(dnorm(x, input$sim_displacement_mean, input$sim_displacement_sd), add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_adp_release_histogram <- renderPlot({
    rate <- 1/input$sim_adp_release_rate
  
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_adp_release_lower,
                           b = input$sim_adp_release_upper,
                           rate = rate) 
    hist(x, xlab = "Hz", main = "Simulated ADP Release Rate (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            a = input$sim_adp_release_lower,
                            b = input$sim_adp_release_upper,
                            rate = rate),
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_time_off_histogram <- renderPlot({
    rate <- 1/input$sim_time_off_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_time_off_lower,
                           b = input$sim_time_off_upper,
                           rate = rate) 
    hist(x, xlab = "ms", main = "Simulated Time Off Distribution (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            a = input$sim_time_off_lower,
                            b =  input$sim_time_off_upper,
                            rate = rate),
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_atp_binding_histogram <- renderPlot({
    rate <- 1/input$sim_atp_binding_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_atp_binding_lower,
                           b = input$sim_atp_binding_upper,
                           rate = rate) 
    hist(x, xlab = "ms", main = "Simulated Hitch Duration Distribution (n = 100k)", freq = F) 
    curve(truncdist::dtrunc(x,
                            spec = "exp", 
                            rate = rate,
                            a = input$sim_atp_binding_lower, 
                            b = input$sim_atp_binding_upper), 
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  
  output$sim_pi_release_histogram <- renderPlot({
    rate <- 1/input$sim_pi_release_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_pi_release_lower,
                           b = input$sim_pi_release_upper,
                           rate = rate) 
    hist(x, xlab = "ms", main = "Simulated Pi Release Duration Distribution (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            rate = rate,
                            a = input$sim_pi_release_lower,
                            b = input$sim_pi_release_upper), 
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  params <- reactive({
        list('Baseline Population' = list(Mean = input$sim_baseline_mean, 
                                          SD = input$sim_baseline_sd),
             
             'Event Population' = list(Count = input$sim_n_events,
                                        'Mean Displacement' = input$sim_displacement_mean,
                                         SD = input$sim_displacement_sd),
             
             'Pi Release' = list(Include = input$sim_pi_release_include, 
                                 Rate = input$sim_pi_release_rate, 
                                 Lower = input$sim_pi_release_lower, 
                                 Upper = input$sim_pi_release_upper),
             
             'ADP Release' = list(Rate = input$sim_adp_release_rate, 
                                  Lower = input$sim_adp_release_lower, 
                                  Upper = input$sim_adp_release_upper),
             
              Hitch = paste0(input$sim_hitch_size, ' nm'), 
             
             'ATP Binding' = list(Rate = input$sim_atp_binding_rate, 
                                  Lower = input$sim_atp_binding_lower, 
                                  Upper = input$sim_atp_binding_upper),
             
              'Time Off' = list(Rate = input$sim_time_off_rate, 
                                Lower = input$sim_time_off_lower, 
                                Upper = input$sim_time_off_upper),
           
             Hz = input$sim_hz,
             Signal = input$sim_signal
    )
  })
  output$sim_parameters <- renderPrint({
    # cat("Current Simulation Parameters: \n",
    #     "Baseline: \n",
    #     " Mean = ", paste0(input$sim_baseline_mean), "\n",
    #     " SD = ", paste0(input$sim_baseline_sd)
       str(params())
  })
  
  output$sim_pi_release_rate_conversion <- renderText({
    rate <- 1/input$sim_pi_release_rate
    paste0("1/Hz = ", rate) 
  })
  
  output$sim_adp_release_conversion <- renderText({
    rate <- 1/input$sim_adp_release_rate
    paste0("1/Hz = ", rate) 
  })
  
  output$sim_atp_binding_conversion <- renderText({
    rate <- 1/input$sim_atp_binding_rate
    paste0("1/Hz = ", rate) 
  })
  
  output$sim_time_off_conversion <- renderText({
    rate <- 1/input$sim_time_off_rate
    paste0("1/Hz = ", rate) 
  })
  
  
  sim_data <- eventReactive(input$sim_go, {
    #browser()
    simulate_single_molecule_trap_data(n = input$sim_n_events, 
                                       hz = input$sim_hz, 
                                       signal_to_noise = input$sim_signal,
                                       baseline = list(mean = input$sim_baseline_mean, 
                                                       sd = input$sim_baseline_sd),
                                       displacement = list(mean = input$sim_displacement_mean, 
                                                           sd = input$sim_displacement_sd),
                                       pi_release = list(rate = input$sim_pi_release_rate, 
                                                         lower = input$sim_pi_release_lower, 
                                                         upper = input$sim_pi_release_upper,
                                                         occurs = input$sim_pi_release_occurs),
                                       adp_release = list(rate = input$sim_adp_release_rate, 
                                                          lower = input$sim_adp_release_lower, 
                                                          upper = input$sim_adp_release_upper,
                                                          hitch = input$sim_hitch_size),
                                       atp_binding = list(rate = input$sim_atp_binding_rate, 
                                                          lower = input$sim_atp_binding_lower, 
                                                          upper = input$sim_atp_binding_upper),
                                       time_off <- list(rate = input$sim_time_off_rate, 
                                                        lower = input$sim_time_off_lower, 
                                                        upper = input$sim_time_off_upper))
  })
  
  output$sim <- dygraphs::renderDygraph({
    dygraphs::dygraph(data.frame(Datapoints = sim_data()$time, nm = sim_data()$data)) %>% 
      dygraphs::dySeries("nm", color = "black") %>% 
      dygraphs::dyRangeSelector()
  })
  
  observeEvent(input$sim_save, {
    defend_if_empty(f$project_input, ui = "Please select a 'Project' folder.", type = "error")
    defend_if_blank(f$project_input, ui = "Please select a 'Project' folder.", type = "error")
    allow_if(grepl("simulation", tolower(f$project_input)), ui = "The 'Project' folder must have 'simulation' in its name to save simulated data to it.")
    defend_if_blank(f$conditions_input, ui = "Please select a 'Conditions folder.", type = "error")
    allow_if(grepl("simulation", tolower(f$conditions_input)), ui = "The 'Conditions' folder must have 'simulation' in its name to save simulated data to it.")
    defend_if_blank(f$date_input, message = "Please select a 'Date' Folder.", type = "error")
    allow_if(is.data.frame(sim_data()), ui = "No simulation data to save")
    withProgress(message = "Saving Simulation Data", {
     num_obs_folders <- nrow(list_files(f$date$path)) + 1
     if(num_obs_folders < 10){
       obs_name <- paste0("obs-0", num_obs_folders)
     } else {
       obs_name <- paste0("obs-", num_obs_folders)
     }
     setProgress(0.5, detail = "Preparing Data")
     data_to_save <- sim_data() %>% 
       dplyr::mutate(project = f$project_input,
                conditions = f$conditions_input,
                date = f$date_input,
                obs = obs_name, 
                include = TRUE, 
                mv2nm = 1,
                nm2pn = 1,
                processed_bead = data,
                processor = "sim",
                report = "not run",
                analyzer = NA,
                review = NA)
     sim_save_folder <- file.path(f$date$path, obs_name)
     setProgress(0.9, detail = "Writing")
     dir.create(sim_save_folder)
     data.table::fwrite(data_to_save, file = file.path(sim_save_folder, "trap-data.csv"))
    })
    showNotification(ui = "Simulation data saved", type = "message")
    
  })
  
  
}

## To be copied in the UI
# mod_split_obs_ui("split_obs")

## To be copied in the server
# callModule(mod_split_obs_server, "split_obs")
 
