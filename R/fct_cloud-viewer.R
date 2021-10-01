#' Upload a project to be used with lasertrapr cloud
#' @description Originally created for use with the lasertrapr cloud viewer, but can be used to backup data to goolge drive as well. 
#' If local changes are made and you want to update the google drive archive, running this function again will trash the old project folder on googledrive
#' and recreate it from scratch. Data is placed in a shared drive called "lasertrapr-cloud" on googledrive. If this folder does not exist it will be created.
#' 
#' @param project a character string. a lasertrapr project folder.
#' @param email a character string. email account associated with your google drive.
#'
#' @return nothing
#' @export
#'
#' @examples push_project_to_lasertrapr_cloud("project_something", "myemail")
push_project_to_lasertrapr_cloud <- function(project, email){
  
  orig_wd <- getwd()
 
  googledrive::drive_auth(email, cache = "~/lasertrapr/.secrets")
  
  lasertrapr_drive <- shared_drive_get("lasertrapr-cloud")
  if(nrow(lasertrapr_drive) == 0){
    lasertrapr_drive <- shared_drive_create("lasertrapr-cloud")
  }
  
  project_folder <- file.path(path.expand("~"), "lasertrapr", project)
  
  options <- list.files(project_folder, 
                        pattern = "options.csv", 
                        recursive = TRUE, 
                        full.names = TRUE)
  
  included <-
    map_df(options, data.table::fread) %>% 
    dplyr::filter(include == TRUE, report == "success", review == TRUE) 
  
  folders <- 
    included %>% 
    dplyr::select(conditions, date, obs)
  
  temp_project <- file.path(tempdir(), Sys.time(), project)
  dir.create(temp_project, recursive = TRUE)
  
  drive_project <- drive_mkdir(project, path = lasertrapr_drive, overwrite = TRUE)
  
  for(i in 1:nrow(folders)){
    conditions <- folders$conditions[[i]]
    date <- as.character(folders$date[[i]])
    obs <- folders$obs[[i]]
    
    drive_conditions <- drive_ls(drive_project)
    if(!conditions %in% drive_conditions$name){
      drive_condition_path <- drive_mkdir(conditions, drive_project)
    } else {
      drive_condition_path <-
        drive_conditions %>% 
        dplyr::filter(name == conditions)
    }
    
    drive_dates <- drive_ls(drive_condition_path)
    if(!date %in% drive_dates$name){
      drive_date_path <- drive_mkdir(date, drive_condition_path)
    } else {
      drive_date_path <-
        drive_dates %>% 
        dplyr::filter(name == date)
    }
    
    drive_obs_all <- drive_ls(drive_date_path)
    if(!obs %in% drive_obs_all$name){
      drive_obs_path <- drive_mkdir(obs, drive_date_path)
    } else {
      drive_obs_path <-
        drive_obs_all %>% 
        dplyr::filter(name == obs)
    }
    
    temp_obs_path <- file.path(temp_project, conditions, date, obs)
    dir.create(temp_obs_path, recursive = TRUE)
    
    files <- list.files(file.path(project_folder, conditions, date, obs))
    
    trap_zip_path <- file.path(temp_obs_path, "trap.zip")
    real_obs_path <- file.path(project_folder, conditions, date, obs)
    setwd(real_obs_path)
    
    zip(trap_zip_path, files)
    drive_upload(trap_zip_path, drive_obs_path)
    
  }
  setwd(orig_wd)
}

#' Reads data from google drive for the cloud viewer
#' @noRd
drive_read_trap <- function(obs){
  incProgress(0.25)
  trap_zip <- drive_ls(obs$id, pattern = "trap.zip")
  
  incProgress(0.25)
  data <- drive_download(trap_zip, path = file.path(tempdir(), paste0(Sys.time(), "trap.zip")))
  
  incProgress(0.25)
  extract_here <- file.path(tempdir(), Sys.time(), "extract_here")
  dir.create(extract_here, recursive = TRUE)
  local_data <- unzip(data$local_path, exdir = extract_here)
  files_to_read <- local_data[grep("*options*|*trap-data*|*measured-events*", local_data)]
  
  data <- lapply(files_to_read, fread)
  names(data) <- c("events", "options", "trap")
  incProgress(0.25)
  return(data)
}


#' Shiny app for viewing lasertrapr data in the "cloud"
#' @description This is a one line shiny app. Literally, all that is required is to 
#' make a new shiny app project and in the app.R call this single function and then push
#' the app into production. Requires putting data in a lasertrapr-cloud shared google drive which 
#' can be accomplished with lasertrapr::push_project_to_lasertrapr_cloud(). The shared drive will be 
#' created automatically if it does not exist. 
#' @param email a character string. Email associated with your google drive account. 
#' @export
lasertrapr_cloud <- function(email){
  
  library(shiny)
  library(dygraphs)
  library(shinycssloaders)
  library(googledrive)
  library(data.table)
  library(magrittr)
  library(tidyverse)
  
  googledrive::drive_auth(email = email, cache = ".secrets")
  
  lasertrapr_drive <- shared_drive_get("lasertrapr-cloud")
  
  if(nrow(lasertrapr_drive) == 0){
    lasertrapr_drive <- shared_drive_create("lasertrapr-cloud")
  }
  
  projects <- drive_ls(shared_drive = lasertrapr_drive, recursive = FALSE, pattern = "project")
  
  ui <- fluidPage(
    
    titlePanel("lasertrapr Cloud"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   uiOutput("projects")
                   ,
                   conditionalPanel("input.project != 'Choose...'",
                                    uiOutput("conditions")
                   ),
                   conditionalPanel("input.conditions != 'Choose...'",
                                    uiOutput("dates")
                   ),
                   conditionalPanel("input.date != 'Choose...'",
                                    uiOutput("obs")
                   ),
                   conditionalPanel("input.obs != null",
                                    actionButton("go", "Go")
                   )
      ),
      mainPanel(width = 9,
                dygraphs::dygraphOutput('graph') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
      )
    )
  )
  
  server <- function(input, output) {
    
    #### file selectors ####
    
    output$projects <- renderUI({
      req(projects)
      selectInput("project",
                  "Project",
                  c("Choose...", projects$name))
    })
    
    conditions <- reactive({
      req(input$project != "Choose...")
      selected_project <- dplyr::filter(projects, name == input$project)
      drive_ls(selected_project$id)
    })
    
    output$conditions <- renderUI({
      req(input$project != "Choose...")
      selectInput("condition",
                  "Conditions",
                  c("Choose...", conditions()$name))
    })
    
    dates <- reactive({
      req(input$condition != "Choose...")
      selected <- dplyr::filter(conditions(), name == input$condition)
      drive_ls(selected$id)
    })
    
    output$dates <- renderUI({
      req(input$condition != "Choose...")
      selectInput("date",
                  "Date",
                  c("Choose...", dates()$name))
    })
    
    obs <- reactive({
      req(input$date != "Choose...")
      selected <- dplyr::filter(dates(), name == input$date)
      drive_ls(selected$id)
    })
    
    output$obs <- renderUI({
      req(input$date != "Choose...")
      obs <- dplyr::filter(obs(), str_detect(name, "obs")) %>% arrange(name)
      selectInput("obs",
                  "Obs",
                  c("Choose...", obs$name))
    })
    
    #### read selected data ####
    
    rv <- reactiveValues()
    observeEvent(input$go, {
      withProgress(message = "Downloading", detail = "This may take a moment...", {
        obs <- dplyr::filter(obs(), name == input$obs)
        rv$data <- drive_read_trap(obs)
      })
      showNotification("Data Downloaded...")
    })
    
    output$graph <- 
      renderDygraph({
        validate(need(rv$data$trap, message = "Please Select Data to View"))
        hz <- rv$data$options$hz[[1]]
        d <- data.frame(index = (1:nrow(rv$data$trap)/hz),
                        raw = rv$data$trap$processed_bead,
                        model = rv$data$trap$hm_overlay)
        
        periods_df <- data.frame(start = rv$data$events$cp_event_start_dp/hz,
                                 stop = rv$data$events$cp_event_stop_dp/hz,
                                 keep = rv$data$events$keep,
                                 event_user_excluded = rv$data$events$event_user_excluded, 
                                 color = scales::alpha("#D95F02" , 0.4))
        
        periods_df %<>%  dplyr::filter(keep == T, event_user_excluded == F)
        
        pni <-  rv$data$events$peak_nm_index
        labels <- rv$data$events %>% filter(keep == T, event_user_excluded == F)
        
        
        dygraphs::dygraph(d) %>% 
          dygraphs::dySeries('raw', color = 'black') %>%
          dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) %>%
          dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') %>%
          add_shades(periods_df) %>% #raw_periods
          #add_shades(excluded_events, color = "#BDBDBD") %>%
          add_labels_hmm(labels, labelLoc = 'bottom') %>% #results$events
          dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
          dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) %>%
          dygraphs::dyUnzoom()
      })
  }
  
  shinyApp(ui, server)
  
}