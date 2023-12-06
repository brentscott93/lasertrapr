#' split_obs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinyFiles
#' @importFrom shiny NS tagList 

mod_split_obs_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(box(width = 3,
                 collapsible = TRUE, collapsed = FALSE,
                 title = "Upload Data",
                 shinyWidgets::radioGroupButtons(
                                 inputId = ns("upload_method"),
                                 label = 'Method',
                                 choices = c("Upload" = "upload",
                                             "Split Obs" = "split_obs"),
                                 direction = "horizontal",
                                 width = "100%",
                                 justified = TRUE,
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: black"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: black"))
                               ),

                 conditionalPanel(
                   condition = " input.upload_method == 'upload'", ns = ns,

                   fluidRow(
                     column(12,
                            shinyFiles::shinyFilesButton(ns("file_input"),
                                                         label = "Browse for file...",
                                                         title = "Select one or more file",
                                                         multiple = TRUE,
                                                         style = "width: 100%; margin-bottom: 5px;"),
                            )
                   ),
                   fluidRow(
                     column(6,

                            shinyWidgets::radioGroupButtons(
                                            inputId = ns("channels"),
                                            label = 'Number of Channels',
                                            choices = c(1, 2),
                                            direction = "horizontal",
                                            width = "100%",
                                            justified = TRUE,
                                            checkIcon = list(
                                              yes = tags$i(class = "fa fa-check-square",
                                                           style = "color: black"),
                                              no = tags$i(class = "fa fa-square-o",
                                                          style = "color: black"))
                                          )),
                     column(6,style =  "padding-top: 32px;",
                            shinyWidgets::prettyCheckbox(ns("in_header"),
                                                         "Cal in header?",
                                                         value = FALSE,
                                                         outline = TRUE,
                                                         shape = "curve",
                                                         status = "primary" ))

                   ),


                   conditionalPanel(
                     condition = "input.in_header == true", ns = ns,
                     fluidRow(
                       column(6,
                              numericInput(ns("header_size"),
                                           label = "Header Size",
                                           value = 68,
                                           step = 1)),
                       column(6,
                              numericInput(ns("header_hz"),
                                           label = "Hz",
                                           value = 15,
                                           step = 1))
                     ),
                     fluidRow(
                       column(6,
                              numericInput(ns("header_nm_v1"),
                                           label = "nm/V",
                                           value = 22,
                                           step = 1,
                                           width = "100%")),
                       column(6,
                              numericInput(ns("header_pn_nm1"),
                                           label = "pN/nm",
                                           value = 18,
                                           step = 1,
                                           width = "100%")
                              )
                     ),


                     conditionalPanel(
                       condition = "input.channels == 2", ns = ns,
                       fluidRow(
                         column(6,

                                numericInput(ns("header_nm_v2"),
                                             label = "nm/V 2",
                                             value = 24,
                                             step = 1,
                                             width = "100%"),
                                ),
                         column(6,

                                numericInput(ns("header_pn_nm2"),
                                             label = "pN/nm 2",
                                             value = 20,
                                             step = 1,
                                             width = "100%")
                                )
                       ),
                       fluidRow(
                         column(6,

                                numericInput(ns("trap1_col"),
                                             label = "Trap 1 Col",
                                             value = 1,
                                             step = 1,
                                             width = "100%"),
                                ),
                         column(6,

                                numericInput(ns("trap2_col"),
                                             label = "Trap 2 Col",
                                             value = 3,
                                             step = 1,
                                             width = "100%")
                                )
                       ),
                       fluidRow(
                         column(6,
                           numericInput(
                           inputId = ns("feedback_motor_bead"),
                           label = "Feedback Motor Bead",
                           value = 30
                         )
                         )
                       )
                     ) # conditional panel input.channels ==2
                   ), # conditional panel header
                   
                   conditionalPanel(
                     condition = "input.in_header == false", ns = ns,
                     fluidRow(
                       column(6,
                              numericInput(ns("hz"), "Hz", 0)
                              ),
                       column(6,
                              div(style = "padding-top: 32px;",
                                  shinyWidgets::prettyCheckbox(ns("ready_for_analysis"),
                                                               "Ready for analysis?",
                                                               value = FALSE,
                                                               outline = TRUE,
                                                               shape = "curve",
                                                               status = "primary" )

                                  ))
                     ),
                     
                     conditionalPanel(
                       condition = " input.ready_for_analysis == true", ns = ns,
                       numericInput(ns("nm_to_pn"), "Stiffness Conversion (pN/nm)", value = 0.04)
                     )
                   ),

                   numericInput(ns("downsample"), "Downsample By (factor)", value = 1),
                   actionButton(ns("simple_upload_button"),
                                "Initialize Data",
                                width = "100%",
                                icon = icon("play-circle"),
                                style = 'margin-top: 25px;')
                 ), #conditional close
                 conditionalPanel(
                   condition = " input.upload_method == 'split_obs'", ns = ns,

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

                   actionButton(inputId = ns("split_obs_button"),
                                label = "Make Observations",
                                icon = icon("eye"),
                                width = "100%",
                                style = 'margin-top: 25px;')
                 ) #conditional close

                 ), #box close,

             box(title = "Simulate Data", width = 9, collapsible = T, collapsed = T,
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
                          dygraphs::dygraphOutput(ns('sim')) |> shinycssloaders::withSpinner(type = 8, color = "#373B38")
                          )
                 )
                 )
             ),

    fluidRow(
      box(title = 'Step Calibration', width = 6, collapsible = T, collapsed = T,
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
                          actionButton(ns('step_button'), 'Step Cal', width = '100%', style = 'margin-top: 25px;'),
      tags$style(".small-box.bg-yellow { background-color: #1B9E77 !important; color: #f2f2f2 !important; }"),
      valueBoxOutput(ns("step_cal_valueBox"), width = 2),
                          ),

                   column(9,
                          plotOutput(ns('step'), width = '100%', height = '275px') |>
                          shinycssloaders::withSpinner(type = 8, color = "#373B38"))
                   )

          ), #box close

      box(title = 'Equipartition', width = 6, collapsible = T, collapsed = T,
          fluidRow(
            column(3, fileInput(ns('equi_file'),
                                'Upload Equi File (.txt)',
                                placeholder = 'Equi.txt',
                                accept = '.txt'),
                   sliderInput(ns('equi_mv2nm'),
                               'mV to nm conversion',
                               min = 1,
                               max = 100,
                               value = 30,
                               step = 1,
                               ticks = F,
                               width = '100%'),

                   withMathJax(helpText("$$\\alpha_{trap}=\\frac{k_B*T_k}{\\sigma^2}$$")),
                   actionButton(ns('equi_button'),
                                'Equi Cal',
                                width = '100%',
                                style = 'margin-top: 25px;'),
      valueBoxOutput(ns("equipartition_valueBox"), width = 2)),
            column(9,
                   plotOutput(ns('equi'), width = '100%', height = '275px') |>
                   shinycssloaders::withSpinner(type = 8, color = "#373B38"))
          )
          )


    ) #row close
  ) #tagList

  
}
    
#' split_obs Server Function
#'
#' @noRd 
#' @import shinyFiles
mod_split_obs_server <- function(input, output, session, f){
    ns <- session$ns
    
    shinyFileChoose(input = input,
                    id = "file_input",
                    roots = c(home="/home"),
                    defaultRoot = "home",
                    defaultPath = "",
                    session = session)



    ## header cal info
    ## these values correspond to the LINE NUMBERS where the info can be found in the headers of the data files
    h <- reactiveValues(header_size = 0,
                        hz = 0,
                        nm_v1 = 0,
                        nm_v2 = 0,
                        pn_nm1 = 0,
                        pn_nm2 = 0,
                        trap1_col = 0,
                        trap2_col = 0,
                        feedback_motor_bead = 0)
    observe({
          h$header_size <- input$header_size
          h$hz <- input$header_hz
          h$nm_v1 <- input$header_nm_v1
          h$nm_v2 <- input$header_nm_v2
          h$pn_nm1 <- input$header_pn_nm1
          h$pn_nm2 <- input$header_pn_nm2
          h$trap1_col <- input$trap1_col
          h$trap2_col <- input$trap2_col
          h$feedback_motor_bead <- input$feedback_motor_bead
       })

      
    observeEvent(input$simple_upload_button, {
      
        defend_if_empty(f$project, "No 'Project' folder selected. Please select a folder with the folder chooser above.")
        defend_if_empty(f$conditions, "No 'Conditions' folder selected. Please select a folder with the folder chooser above.")
        defend_if_empty(f$date, "No 'Date' folder selected. Please select a folder with the folder chooser above.")
        ## defend_if_equal(input$hz == 0, "Please enter sampling frequency, Hz.")
        req(nchar(f$date$path>0))

        
        input_data <- parseFilePaths(c(home="/home"), input$file_input)
        if(input$in_header){
            

            upload_data_cal_in_header(input_data = input_data,
                                      h = h,
                                      project = f$project,
                                      conditions = f$conditions,
                                      date = f$date,
                                      number_of_channels = input$channels,
                                      downsample_by = input$downsample_by)

        } else {

            
            simple_upload(input_data = input$simple_data_input,
                          project = f$project,
                          conditions = f$conditions,
                          date = f$date,
                          nm2pn = input$nm_to_pn, 
                          ready_for_analysis = input$ready_for_analysis,
                          hz = input$hz,
                          downsample_by = input$downsample_by)
        }
        
        f$new_obs_from_split <- f$new_obs_from_split + 1
    })
         #check if a date folder is properly selected
    observeEvent(input$split_obs_button, {
        ## golem::print_dev("go")
        if(rlang::is_empty(f$date) == TRUE){
            showNotification("No 'Date' folder selected. Please select a folder with the folder chooser above. ",
                             type = "error")
            
        } else if(rlang::is_empty(input$trap_txt_upload)){
            showNotification("No data uploaded",
                             type = "error")
        } else {
            req(nchar(f$date$path>0))
            req(input$trap_txt_upload)
            all_data <- purrr::map(input$trap_txt_upload$name, ~substring(.x, 1, 4) == 'Data')
            
            if(all(all_data != TRUE)){
                showNotification("Not all files are valid 'Data' files. Only upload files starting with 'Data'.",
                                 type = "error")
            } else {
                ## golem::print_dev('before split_obs call')
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
    if(rlang::is_empty(input$equi_file)){
      showNotification('No data uploaded', type = 'error')
    ## } else if(substring(input$equi_file$name, 1, 4) != 'Equi') {
    ##   showNotification("Not a valid 'Equi' file.", type = 'error')
    } else {
    withProgress(message = "Equipartition Calibration", min= 0, max = 1, value = 0.01, {
      incProgress(0.25, detail = "Reading Data")
      files <- data.table::fread(input$equi_file$datapath, col.names = c('bead', 'trap')) |>
        dplyr::mutate(bead = bead * input$equi_mv2nm) |>
        dplyr::pull(bead)
      mean_equi <- mean(files)
      equi_data <- files - mean_equi
      e$vector <- equi_data
      incProgress(0.75, detail = "Calculating")
      e$cal <-  equipartition(equi_data)
    
    })
    }
  })


  
  output$equi <- renderPlot( {
    req(e$vector)
    plot(e$vector, ylab = 'nm', xlab = 'Datapoints', type = 'l')
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
    if(rlang::is_empty(input$step_files)){
      showNotification('No data uploaded', type = 'error')
    } else if(substring(input$step_files$name, 1, 4) != 'Step') {
      showNotification("Not a valid 'Step' file.", type = 'error')
    }
  })
    
  
  step_calibration <- eventReactive(input$step_button, {
    req(input$step_files$datapath)
    ## req(substring(input$step_files$name, 1, 4) == 'Step')
    withProgress(message = "Step Calibration", min= 0, max = 1, value = 0.01, {
      incProgress(0.4, detail = "Reading Data")
      files <- purrr::map(input$step_files$datapath, fread, col.names = c('bead', 'trap')) |>
        purrr::map(dplyr::pull, bead)
      incProgress(0.75, detail = "Calculating...This may take a while...")
      steps <- purrr::map(files, step_cal, step = input$step_cal_stepsize)
      incProgress(1, detail = "Done!")
    })
    return(steps)
  })
  
  conversion <- reactive({
    conv1 <- purrr::map(step_calibration(), "mv2nm_conversion")
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
    
    grobs <- purrr::map(step_calibration(), "plot")
    p <- gridExtra::grid.arrange(grobs = grobs, ncol = 1)
    
    return(p)
    
  })
  
  
  output$step <- renderPlot({
    step_calibration_plot()
  })
  
  
  sim <- reactiveValues(baseline_mean = 0,
                        baseline_sd = 8, 
                        step = 5,
                        step_sd =  8,
                        pi_release = "after",
                        pi_release_rate = 200, 
                        pi_release_lower = 1/1000,
                        pi_release_upper = 1,
                        adp_release = "set_time",
                        adp_release_rate = 20,
                        adp_release_lower = 1/1000,
                        adp_release_upper = 1,
                        hitch_size = 2,
                        atp_binding = "set_time",
                        atp_binding_rate = 50,
                        atp_binding_upper = 1,
                        atp_binding_lower = 20/1000,
                        time_off_rate = 1,
                        time_off_upper = 10000/1000,
                        time_off_lower = 100/1000)
  observeEvent(input$sim_ok, {
    sim$baseline_mean = input$sim_baseline_mean
    sim$baseline_sd = input$sim_baseline_sd 
    
    sim$step = input$sim_displacement_mean
    sim$step_sd = input$sim_displacement_sd
    
    sim$pi_release = input$sim_pi_release_occurs
    sim$pi_release_rate = input$sim_pi_release_rate
    sim$pi_release_lower = input$sim_pi_release_lower
    sim$pi_release_upper = input$sim_pi_release_upper
    
   # sim$adp_release = sim_adp_release_type
    sim$adp_release_rate = input$sim_adp_release_rate
    sim$adp_release_lower = input$sim_adp_release_lower
    sim$adp_release_upper = input$sim_adp_release_upper
    sim$hitch_size = input$sim_hitch_size
    
    
    sim$atp_binding_rate = input$sim_atp_binding_rate
    sim$atp_binding_upper = input$sim_atp_binding_upper
    sim$atp_binding_lower = input$sim_atp_binding_lower
    
    sim$time_off_rate = input$sim_time_off_rate
    sim$time_off_upper = input$sim_time_off_upper
    sim$time_off_lower = input$sim_time_off_lower
    
   removeModal()
  })
  observeEvent(input$sim_options, {
    showModal(
      modalDialog(
        size = "l",
       title = "Define Simulation Parameters",
       footer = tagList(modalButton("Cancel"), actionButton(ns("sim_ok"), "OK")),
       tabsetPanel(
         tabPanel("Baseline",
          sliderInput(ns("sim_baseline_mean"), "Mean", value = sim$baseline_mean, step = 1, round = TRUE, min = -50, max = 50, width = "100%"),
          sliderInput(ns("sim_baseline_sd"), "SD", value = sim$baseline_sd, step = 1, round = TRUE, min = 0, max = 15,  width = "100%"),
          plotOutput(ns("sim_baseline_histogram"))
          ),
          tabPanel("Displacements",
           sliderInput(ns("sim_displacement_mean"), "Mean", value = sim$step, step = 1, round = TRUE, min = -50, max = 50, width = "100%"),
           sliderInput(ns("sim_displacement_sd"), "SD", value = sim$step_sd, step = 1, round = TRUE, min = 0, max = 15, width = "100%"),
           plotOutput(ns("sim_displacement_histogram"))
          ),
         tabPanel("Pi Release",
           br(),
           shinyWidgets::radioGroupButtons(
             inputId = ns('sim_pi_release_occurs'),
             label = "Pi release before or after stroke?",
             choices = c("Before" = "before",
                         "After" = "after",
                         "Uncoupled" = "uncoupled"),
             justified = TRUE,
             selected = sim$pi_release,
             checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square",
                              style = "color: black"),
                              no = tags$i(class = "fa fa-square-o",
                              style = "color: black"))
                  ),
           conditionalPanel(condition = "input.sim_pi_release_occurs != 'uncoupled'", ns = ns, 
            fluidRow(
              column(6,
               numericInput(ns("sim_pi_release_rate"), "Avg Rate (Hz)", value = sim$pi_release_rate, max = 500, min = 0, step = 5, width = "100%")
              ), 
              column(6, 
                     div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_pi_release_rate_conversion")))
              )
            ),
            sliderInput(ns("sim_pi_release_lower"),
                        "Lower (ms)",
                        value = sim$pi_release_lower, 
                        step = 1/1000, 
                        round = TRUE, 
                        min = 1/1000, 
                        max = 50/1000, 
                        width = "100%"),
            sliderInput(ns("sim_pi_release_upper"), "Upper (ms)", 
                        value = sim$pi_release_upper, 
                        step = 1/1000, 
                        round = TRUE, 
                        min = 0/1000, 
                        max = round(max(rexp(10000, sim$pi_release_rate)), 3), 
                        width = "100%"),
            plotOutput(ns("sim_pi_release_histogram"))
           )
         ),
           tabPanel("ADP Release",
          #          shinyWidgets::radioGroupButtons(
          #            inputId = ns('sim_adp_release_type'),
          #            label = "",
          #            choices = c("Use Distribution" = "distribution",
          #                        "Set Time" = "set_time"),
          #            justified = TRUE,
          #            selected = sim$adp_release,
          #            checkIcon = list(
          #              yes = tags$i(class = "fa fa-check-square",
          #                           style = "color: black"),
          #              no = tags$i(class = "fa fa-square-o",
          #                          style = "color: black"))
          #          ),
              fluidRow(
              # conditionalPanel(condition = "input.sim_adp_release_type == 'distribution'", ns = ns, 
                  column(4,
                    numericInput(ns("sim_adp_release_rate"), "Avg Rate (Hz)", value = sim$adp_release_rate, max = 500, min = 0, step = 5, width = "100%"),
                ), 
                  column(4, 
                      div(style = 'margin-top: 22px;',  verbatimTextOutput(ns("sim_adp_release_conversion")))
                ),
             # ),
              # conditionalPanel(condition = "input.sim_adp_release_type == 'set_time'", ns = ns, 
              #                  column(8, 
              #                         sliderInput(ns("sim_adp_release_set_time"), label = "Set Time",  min = 0, max = 1000, value = sim$adp_release_rate, width = "100%")
              #                        
              #                  )
              # ), 
               column(4, 
                      numericInput(ns("sim_hitch_size"), "Hitch Size (nm)", value = sim$hitch_size, step = 0.5,  min = 0, max = 10,  width = "100%")
              )
             ),
            # conditionalPanel(condition = "input.sim_adp_release_type == 'distribution'", ns = ns, 
               sliderInput(ns("sim_adp_release_lower"), 
                           "Lower (ms)", 
                           value = sim$adp_release_lower,
                           step = 1/1000, 
                           round = TRUE, 
                           min = 0, 
                           max = 50/1000, 
                           width = "100%"),
               sliderInput(ns("sim_adp_release_upper"), 
                           "Upper (ms)",
                           value = sim$adp_release_upper,
                           step = 10/1000,
                           round = TRUE, 
                           min = 0,
                           max = round(max(rexp(10000,sim$adp_release_rate)), 3), 
                           width = "100%"),
               
               plotOutput(ns("sim_adp_release_histogram"))
           #  )
          ),
         tabPanel("ATP Binding",
            # shinyWidgets::radioGroupButtons(
            #         inputId = ns('sim_atp_binding_type'),
            #         label = "",
            #         choices = c("Use Distribution" = "distribution",
            #                     "Set Time" = "set_time"),
            #         justified = TRUE,
            #         selected = sim$atp_binding,
            #         checkIcon = list(
            #           yes = tags$i(class = "fa fa-check-square",
            #                        style = "color: black"),
            #           no = tags$i(class = "fa fa-square-o",
            #                       style = "color: black"))
            #       ),
          #  conditionalPanel(condition = "input.sim_atp_binding_type == 'distribution'", ns = ns, 
             fluidRow(
               column(6,
                numericInput(ns("sim_atp_binding_rate"), 
                             "Rate (1/mean)", 
                             value = sim$atp_binding_rate,
                             max = 1000, 
                             min = 0, 
                             step = 5,
                             width = "100%")
               ),
                column(6, 
                       div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_atp_binding_conversion")))
                 ),
              ),
             sliderInput(ns("sim_atp_binding_lower"),
                         "Lower (ms)",
                         value = sim$atp_binding_lower, 
                         step = 1/1000,
                         round = TRUE,
                         min = 0, 
                         max = 100/1000, 
                         width = "100%"),
             sliderInput(ns("sim_atp_binding_upper"), 
                         "Upper (ms)",
                         value = sim$atp_binding_upper, 
                         step = 10/1000, 
                         round = TRUE, 
                         min = 0, 
                         max = round(max(rexp(10000, sim$atp_binding_rate)), 3), 
                         width = "100%"),
             plotOutput(ns("sim_atp_binding_histogram"))
          #  ), 
          #   conditionalPanel(condition = "input.sim_atp_binding_type == 'set_time'", ns = ns, 
          #      sliderInput(ns("sim_atp_binding_set_time"), label = "Set Rate",  min = 1, max = 5000, value = sim$atp_binding_rate, width = "100%")
          # )
         ),
         
         tabPanel("Time Off",
          fluidRow(
           column(6,
            numericInput(ns("sim_time_off_rate"), 
                         "Rate (1/mean)", 
                         value = sim$time_off_rate,
                         max = 10, 
                         min = 0,
                         step = 0.5,
                         width = "100%")
           ),
           
           column(6, 
                  div(style = 'margin-top: 22px;', verbatimTextOutput(ns("sim_time_off_conversion")))
           )
          ),
          sliderInput(ns("sim_time_off_lower"),
                      "Lower (ms)",
                      value = sim$time_off_lower, 
                      step = 1/1000,
                      round = TRUE,
                      min = 0,
                      max = 100/1000,
                      width = "100%"),
          sliderInput(ns("sim_time_off_upper"),
                      "Upper (ms)", 
                      value = sim$time_off_upper,
                      step = 100/1000,
                      round = TRUE, 
                      min = 0, 
                      max = round(max(rexp(10000, sim$time_off_rate)), 3),
                      width = "100%"),
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
    rate <- input$sim_adp_release_rate
  
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_adp_release_lower,
                           b = input$sim_adp_release_upper,
                           rate = rate) 
    hist(x, xlab = "Seconds", main = "Simulated ADP Release Rate (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            a = input$sim_adp_release_lower,
                            b = input$sim_adp_release_upper,
                            rate = rate),
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_time_off_histogram <- renderPlot({
    rate <- input$sim_time_off_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_time_off_lower,
                           b = input$sim_time_off_upper,
                           rate = rate) 
    hist(x, xlab = "Seconds", main = "Simulated Time Off Distribution (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            a = input$sim_time_off_lower,
                            b =  input$sim_time_off_upper,
                            rate = rate),
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  output$sim_atp_binding_histogram <- renderPlot({
    rate <- input$sim_atp_binding_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_atp_binding_lower,
                           b = input$sim_atp_binding_upper,
                           rate = rate) 
    hist(x, xlab = "Seconds", main = "Simulated Hitch Duration Distribution (n = 100k)", freq = F) 
    curve(truncdist::dtrunc(x,
                            spec = "exp", 
                            rate = rate,
                            a = input$sim_atp_binding_lower, 
                            b = input$sim_atp_binding_upper), 
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  
  output$sim_pi_release_histogram <- renderPlot({
    rate <- input$sim_pi_release_rate
    x <- truncdist::rtrunc(100000, 
                           spec = "exp", 
                           a = input$sim_pi_release_lower,
                           b = input$sim_pi_release_upper,
                           rate = rate) 
    hist(x, xlab = "Seconds", main = "Simulated Pi Release Duration Distribution (n = 100k)", freq = F)
    curve(truncdist::dtrunc(x,
                            spec = "exp",
                            rate = rate,
                            a = input$sim_pi_release_lower,
                            b = input$sim_pi_release_upper), 
          add = T, col = "red")
    graphics::box(bty = "l")
  })
  
  params <- reactive({
        list('Baseline Population' = list(Mean = sim$baseline_mean, 
                                          SD = sim$baseline_sd),
             
             'Event Population' = list(Count = input$sim_n_events,
                                        'Mean Displacement' = sim$step,
                                         SD = sim$step_sd),
             
             'Pi Release' = list(Occurs = sim$pi_release, 
                                 Rate = sim$pi_release_rate, 
                                 Lower = sim$pi_release_lower, 
                                 Upper = sim$pi_release_upper),
             
             'ADP Release' = list(Rate = sim$adp_release_rate, 
                                  Lower = sim$adp_release_lower, 
                                  Upper = sim$adp_release_upper),
             
              Hitch = paste0(sim$hitch_size, ' nm'), 
             
             'ATP Binding' = list(Rate = sim$atp_binding_rate, 
                                  Lower = sim$atp_binding_lower, 
                                  Upper = sim$atp_binding_upper),
             
              'Time Off' = list(Rate = sim$time_off_rate, 
                                Lower = sim$time_off_lower, 
                                Upper = sim$time_off_upper),
           
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
 #   browser()
    if(input$sim_pi_release_occurs == "uncoupled"){
      pi_release <- "uncoupled"
    } else {
      pi_release <- list(rate = sim$pi_release_rate, 
                         lower = sim$pi_release_lower, 
                         upper = sim$pi_release_upper,
                         occurs = sim$pi_release)
    }
    
    # if(input$sim_adp_release_type == "set_time"){
    #   adp_release <-  list(set_time = input$sim_adp_release_set_time,
    #                        hitch = input$sim_hitch_size)
    # } else {
      adp_release <-  list(rate = sim$adp_release_rate, 
                           lower = sim$adp_release_lower, 
                           upper = sim$adp_release_upper,
                           hitch = sim$hitch_size)
    #}
    
    # if(input$sim_atp_binding_type == "set_time"){
    #   atp_binding <-  list(set_time = input$sim_atp_binding_set_time)
    # } else {
      atp_binding <-  list(rate = sim$atp_binding_rate, 
                           lower = sim$atp_binding_lower, 
                           upper = sim$atp_binding_upper)
   # }
      
    simulate_single_molecule_trap_data(n = input$sim_n_events, 
                                       hz = input$sim_hz, 
                                       signal_to_noise = input$sim_signal,
                                       baseline = list(mean = sim$baseline_mean, 
                                                       sd = sim$baseline_sd),
                                       displacement = list(mean = sim$step, 
                                                           sd = sim$step_sd),
                                       pi_release = pi_release, 
                                       adp_release = adp_release, 
                                       atp_binding = atp_binding,
                                       time_off = list(rate = sim$time_off_rate, 
                                                        lower = sim$time_off_lower, 
                                                        upper = sim$time_off_upper))
  })
  
  output$sim <- dygraphs::renderDygraph({
    dygraphs::dygraph(data.frame(Datapoints = sim_data()$time, nm = sim_data()$data)) |>
      dygraphs::dySeries("nm", color = "black") |>
      dygraphs::dyRangeSelector()
  })
  
  observeEvent(input$sim_save, {
    defend_if_empty(f$project_input, ui = "Please select a 'Project' folder.", type = "error")
    defend_if_blank(f$project_input, ui = "Please select a 'Project' folder.", type = "error")
    allow_if(grepl("simulation", tolower(f$project_input)), ui = "The 'Project' folder must have 'simulation' in its name to save simulated data to it.")
    defend_if_blank(f$conditions_input, ui = "Please select a 'Conditions folder.", type = "error")
    allow_if(grepl("simulation", tolower(f$conditions_input)), ui = "The 'Conditions' folder must have 'simulation' in its name to save simulated data to it.")
    defend_if_blank(f$date_input, ui = "Please select a 'Date' Folder.", type = "error")
    allow_if(is.data.frame(sim_data()), ui = "No simulation data to save")
    withProgress(message = "Saving Simulation Data", {
     num_obs_folders <- nrow(list_files(f$date$path)) + 1
     if(num_obs_folders < 10){
       obs_name <- paste0("obs-0", num_obs_folders)
     } else {
       obs_name <- paste0("obs-", num_obs_folders)
     }
     setProgress(0.5, detail = "Preparing Data")
     trap_data_to_save <- sim_data() |>
       dplyr::mutate(project = f$project_input,
                conditions = f$conditions_input,
                date = f$date_input,
                obs = obs_name, 
                raw_bead = data, 
                processed_bead = data)
                
     options_to_save <- data.frame(
       project = f$project_input,
       conditions = f$conditions_input,
       date = f$date_input,
       obs = obs_name,
       mv2nm = 1,
       nm2pn = 1,
       include = TRUE,
       processor = "sim",
       report = "not run",
       analyzer = NA,
       review = NA,
       channels = 1,
       hz = input$sim_hz,
       sim_baseline_mean = sim$baseline_mean,
       sim_baseline_sd =  sim$baseline_sd,

       sim_step = sim$step,
       sim_step_sd = sim$step_sd,

       sim_pi_release = sim$pi_release,
       sim_pi_release_rate = sim$pi_release_rate ,
       sim_pi_release_lower =  sim$pi_release_lower,
       sim_pi_release_upper = sim$pi_release_upper,

       sim_adp_release = sim$adp_release,
       sim_adp_release_rate =  sim$adp_release_rate,
       sim_adp_release_lower = sim$adp_release_lower,
       sim_adp_release_upper = sim$adp_release_upper,
       sim_hitch_size =  sim$hitch_size,


       sim_atp_binding_rate = sim$atp_binding_rate,
       sim_atp_binding_upper =   sim$atp_binding_upper,
       sim_atp_binding_lower =  sim$atp_binding_lower,

       sim_time_off_rate =  sim$time_off_rate,
       sim_time_off_upper = sim$time_off_upper,
       sim_time_off_lower = sim$time_off_lower)
     
     sim_save_folder <- file.path(f$date$path, obs_name)
     setProgress(0.9, detail = "Writing")
     dir.create(sim_save_folder)
     filenames <- c("trap-data.csv", "options.csv")
     data_to_save <- list(trap_data_to_save, options_to_save)
     
     purrr::walk2(data_to_save, filenames, ~data.table::fwrite(.x, file = file.path(sim_save_folder, .y)))
     
    })
    showNotification(ui = "Simulation data saved", type = "message")
    
  })
  
  #### file choose ####

  # shinyFiles::shinyFileChoose(
  #   input = input, 
  #   id = "file_select", 
  #   roots= shinyFiles::getVolumes(), 
  #   filetypes=c('', 'txt', 'csv'),
  #   session = session)
  # 
  # 
  # 
  # output$selected_files_to_upload <- renderPrint({
  #   cat(input$file_select)
  # })
}

## To be copied in the UI
# mod_split_obs_ui("split_obs")

## To be copied in the server
# callModule(mod_split_obs_server, "split_obs")
 
