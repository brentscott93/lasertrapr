#' folder_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_folder_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(strong('File Manager')),
    uiOutput(ns('trap_project')),
    conditionalPanel(
      condition = "input.trap_project_selectInput == 'create'", ns = ns,
      textInput(ns("trap_create_project_textInput"), "Name Project"),
      actionButton(ns("trap_create_project_actionButton"), "Create Project")
    ),#conditional panel close
    uiOutput(ns('trap_conditions')),
    conditionalPanel(
      condition = "input.trap_conditions_selectInput == 'create'", ns = ns,
      textInput(ns("trap_create_conditions_textInput"), "Name Conditions"),
      actionButton(ns("trap_create_conditions_actionButton"), "Create Conditions")
    ),#conditional panel close
    
    uiOutput(ns("trap_date")),
    conditionalPanel(
      condition = "input.trap_date_selectInput == 'create'", ns = ns, 
      textInput(ns("trap_create_date_textInput"), "Enter Date", placeholder = "YYYY-MM-DD"),
      actionButton(ns("trap_create_date_actionButton"), "Create Date")
    ),
    
    uiOutput(ns("trap_obs"))
 
  )
}
    
#' folder_manager Server Function
#'
#' @noRd 
#' @import tidyverse magrittr
mod_folder_manager_server <- function(input, output, session, lasertrapr_folder, f){
  ns <- session$ns
  observe({
    f$obs_input <- input$trap_obs_selectInput
    })
  observe({golem::print_dev(input$trap_obs_selectInput)})
  #### CREATE NEW FOLDERS ####
  rv <- reactiveValues()
  rv$new_trap_project <- 0
  observeEvent(input$trap_create_project_actionButton,{
    
    new_trap_project_name <- paste0("project_", input$trap_create_project_textInput)
    dir.create(path = file.path(lasertrapr_folder, new_trap_project_name))
    showNotification("Project folder created", type = "message")
    rv$new_trap_project <- rv$new_trap_project + 1
    
  })
  
  rv$new_trap_condition <- 0
  observeEvent(input$trap_create_conditions_actionButton,{
    
    dir.create(path = file.path(trap_selected_project()$path, input$trap_create_conditions_textInput))
    
    showNotification("Condition folder created", type = "message")
    rv$new_trap_condition <- rv$new_trap_condition + 1
    
  })
  
  
  rv$new_trap_date <- 0
  observeEvent(input$trap_create_date_actionButton,{
    if(substring(input$trap_create_date_textInput, 5, 5) == '-' & substring(input$trap_create_date_textInput, 8, 8) == '-'){
      golem::print_dev('yay')
    dir.create(path = file.path(trap_selected_conditions()$path, input$trap_create_date_textInput))
    
    showNotification("Date folder created", type = "message")
    rv$new_trap_date <- rv$new_trap_date + 1
    } else {
      showNotification('Not a valid date.', type = 'error')
    }
    
  })
  
  
  # END CREATE NEW FOLDERS ON DRIVE
  #--------------------------------------------------------------------------------------------------------
  #### USER SELECTED FOLDERS ####
  
  #list project folders
  project_names <- eventReactive(rv$new_trap_project,{
    list_dir(path = lasertrapr_folder)
  })
  
  #output for renderUI select input button with project names
  output$trap_project = renderUI({
    if(is_empty(project_names) == TRUE){
      selectInput(ns('trap_project_selectInput'), "Select Project", c(Choose='', "Create New..." = "create", selectize = TRUE), width = '500px')
    } else {
      selectInput(ns('trap_project_selectInput'),
                  "Select Project",
                  c(Choose='', "Create New..." = "create", project_names()$name),
                  selectize = TRUE,
                  width ="500px")
    }
  })
  
  #pick the user selected project folder
  trap_selected_project <-  reactive({
    req(project_names())
    project_names() %>%
      filter(name == input$trap_project_selectInput)
    
  })
  
  
  
  observe({ 
    req(input$trap_project_selectInput)
    f$project <- trap_selected_project()
    })
  #CONDITION
  #list of condition names in selected project
 
 observe({
   req(input$trap_project_selectInput)
   rv$conditions_names <-  list_dir(path = trap_selected_project()$path)
  })

 observeEvent(rv$new_trap_condition, ignoreNULL = T, ignoreInit = T, {
   req(input$trap_project_selectInput)
   rv$conditions_names <-  list_dir(path = trap_selected_project()$path)
 })
 
 

  # make select input button with conditions names

  output$trap_conditions <-  renderUI({
    req(input$trap_project_selectInput)
    req(input$trap_project_selectInput != "create")

    selectInput(ns('trap_conditions_selectInput'),
                "Select Conditions",
                c(Choose='', "Create New..." = "create", rv$conditions_names$name),
                selectize = TRUE,
                width = "100%")

  })

  #4 selected condition
  trap_selected_conditions <-  reactive({

    full_paths_con <- list_dir(trap_selected_project()$path)

    user_selection_con <- full_paths_con %>%
      dplyr::filter(name == input$trap_conditions_selectInput)
  })
  
  observe({ 
    req(input$trap_conditions_selectInput)
    f$conditions <-  trap_selected_conditions() 
    })
  
  #DATE
  
  observe({
    req(input$trap_conditions_selectInput)
    rv$date_names <-  list_dir(path = trap_selected_conditions()$path)
  })
  
  observeEvent(rv$new_trap_date, ignoreNULL = T, ignoreInit = T, {
    req(input$trap_conditions_selectInput)
    rv$date_names <-  list_dir(path = trap_selected_conditions()$path)
  })
  
  
  # make select input button with project names
  output$trap_date = renderUI({
    req(input$trap_conditions_selectInput)
    req(input$trap_conditions_selectInput != "create")

    selectInput(ns('trap_date_selectInput'),
                "Select Date",
                c(Choose='', "Create New..." = "create", rv$date_names$name),
                selectize = TRUE,
                width = "100%")
  })
 
  
  #4 selected date
  trap_selected_date <-  reactive({

    full_paths_date <- list_dir(trap_selected_conditions()$path)

    user_selection_date <- full_paths_date %>%
      dplyr::filter(name == input$trap_date_selectInput)

  })

   observe({
    req(input$trap_date_selectInput)
     f$date <- trap_selected_date()
    })
####
  # observe({req(rv$obs_names)
  #   golem::print_dev(rv$obs_names)
  #   })
  observe({
    req(input$trap_date_selectInput)
    req(is_tibble(trap_selected_date()))
    golem::print_dev(trap_selected_date()$path)
    })
  #obs
  observe({
    req(input$trap_date_selectInput)
    rv$obs_names <-  list_dir(path = trap_selected_date()$path) %>% 
                        dplyr::filter(str_detect(name, 'obs'))
  })
  
  observeEvent(f$new_obs, ignoreNULL = T, ignoreInit = T, {
    req(input$trap_date_selectInput)
    rv$obs_names <-  list_dir(path = trap_selected_date()$path)%>%
                           dplyr::filter(str_detect(name, 'obs'))
    updateSelectInput(session,
                      ns('trap_obs_selectInput'),
                         "Select Observation",
                         c(Choose='',rv$obs_names$name),
                         selected = f$current_obs)
    f$new_obs_refresh_graph <- f$new_obs_refresh_graph + 1
  })
  
  observeEvent(f$new_obs_from_split, ignoreNULL = T, ignoreInit = T, {
    req(input$trap_date_selectInput)
    rv$obs_names <-  list_dir(path = trap_selected_date()$path)%>%
      dplyr::filter(str_detect(name, 'obs'))
    updateSelectInput(session,
                      ns('trap_obs_selectInput'),
                      "Select Observation",
                      c(Choose='',rv$obs_names$name))
  })
  
  # make select input button with project names
  output$trap_obs = renderUI({
    req(input$trap_date_selectInput)
    selectInput(ns('trap_obs_selectInput'),
                "Select Observation",
                c(Choose = '', rv$obs_names$name),
                selectize = TRUE,
                width = "100%")
  })
  
  
  #4 selected date
  trap_selected_obs <-  reactive({
    
    full_paths_obs <- list_dir(trap_selected_date()$path)
    
    user_selection_obs<- full_paths_obs %>%
      dplyr::filter(name == input$trap_obs_selectInput)
    
  })
  
  observe({
    req(input$trap_obs_selectInput)
    f$obs <- trap_selected_obs()
  })
  


  
}
    
## To be copied in the UI
# mod_folder_manager_ui("folder_manager_ui_1")
    
## To be copied in the server
# callModule(mod_folder_manager_server, "folder_manager_ui_1")
 
