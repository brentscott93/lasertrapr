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
  
  events <-
    included %>% 
    mutate(path = file.path(path.expand("~"), "lasertrapr", project, conditions, date, obs, "measured-events.csv")) %>% 
    pull(path) %>% 
    map_df(., data.table::fread)
    
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
  
  #upload events
  drive_summary_path <- drive_mkdir("summary", drive_project)
  temp_summary_path <- file.path(temp_project, "all-events.csv")
  data.table::fwrite(events, file = temp_summary_path)
  drive_upload(temp_summary_path, drive_summary_path, type = "spreadsheet")
  
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
  if(data$options$analyzer == "hm/cp"){
    running_data <- local_data[grep("*hm-model-data*", local_data)]
    data$running <- data.table::fread(running_data)
  }
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
#' @examples lasertrapr_cloud("something_at_gmail.com")
lasertrapr_cloud <- function(email){
  
  library(shiny)
  library(dygraphs)
  library(shinycssloaders)
  library(googledrive)
  library(data.table)
  library(magrittr)
  library(tidyverse)
  library(ggstatsplot)
  library(cowplot)
  
  googledrive::drive_auth(email = email, cache = ".secrets")
  
  lasertrapr_drive <- shared_drive_get("lasertrapr-cloud")
  
  if(nrow(lasertrapr_drive) == 0){
    lasertrapr_drive <- shared_drive_create("lasertrapr-cloud")
  }
  
  projects <- drive_ls(shared_drive = lasertrapr_drive, recursive = FALSE, pattern = "project")
  
  ui <- navbarPage(
    id = "tabs",
    title = "Lasertrapr Cloud",
    tabPanel("Login", 
             column(4),
             column(3, passwordInput("login", "Enter Passcode", width = "100%")),
             column(1, actionButton("go_login", "Go", width = "100%", style = "margin-top: 25px")),
             column(4)
    ),
    tabPanel("Summary",
             sidebarPanel(width = 3,
                          uiOutput("projects_summary"),
                          actionButton("go_summary", "Go")
             ),
             mainPanel(
               fluidRow(
                 h4("Event Summary Table"),
                 DT::DTOutput('table') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'),
                 br()
               ),
               fluidRow(
                 tabsetPanel(
                   tabPanel("Displacements", plotOutput('step', height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   tabPanel("Force",  plotOutput('force', height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   tabPanel("Time On",  plotOutput('ton', height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   tabPanel("Time Off", plotOutput('toff', height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'))
                   #tabPanel("ECDF", plotOutput(ns('ecdf'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   #tabPanel("Event Frequency", plotOutput(ns('ef'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   #tabPanel("Stiffness", plotOutput(ns('stiffness'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
                   # tabPanel("Correlations", plotOutput(ns('correlations'), height = '600px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'))
                 )
               )
             )
    ),
    tabPanel("Traces",
             
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
                         plotOutput("mv_plot") %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
               )
             ),
             
             fluidRow(
               dygraphs::dygraphOutput('graph') %>% shinycssloaders::withSpinner(type = 8, color = "#373B38")
             )
    )
  )
  
  server <- function(input, output) {
    ### login ####
    observe({
      if(input$go_login == 0){
        hideTab(inputId = "tabs", target = "Summary")
        hideTab(inputId = "tabs", target = "Traces")
      }
    })
    
    observeEvent(input$go_login, {
      if(input$login == "tr@p"){
        showTab(inputId = "tabs", target = "Summary")
        showTab(inputId = "tabs", target = "Traces")
        hideTab(inputId = "tabs", target = "Login")
      } else {
        hideTab(inputId = "tabs", target = "Summary")
        hideTab(inputId = "tabs", target = "Traces")
      }
    })
    
    
    #### file selectors ####
    
    output$projects <- renderUI({
      req(projects)
      selectInput("project",
                  "Project",
                  c("Choose...", projects$name))
    })
    
    output$projects_summary <- renderUI({
      req(projects)
      selectInput("projects_summary",
                  "Project",
                  c("Choose...", projects$name))
    })
    
    conditions <- reactive({
      req(input$project != "Choose...")
      selected_project <- dplyr::filter(projects, name == input$project)
      drive_ls(selected_project$id) %>% 
        dplyr::filter(str_detect(name, "summary", negate = TRUE))
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
    
    #### read selected trace data ####
    
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
        req(rv$data$trap)
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
    
    output$mv_plot <- 
      renderPlot({
        validate(need(rv$data$trap, message = "Please Select Data to View"))
        mv_data <- rv$data$running
        mv_data$state <- factor(mv_data$state, levels = c(1, 2))
        
        mv1 <- 
          ggplot2::ggplot(mv_data)+
          geom_point(aes(x = run_mean, y = run_var, color = state), size = 3, shape = 16, alpha = 0.5)+
          scale_color_manual(values = c("#1B9E77", "#D95F02"))+
          ggtitle('Mean-Variance (overlayed)')+
          ylab('Variance')+
          xlab('Mean (nm)')+
          theme_linedraw(base_size = 18)+
          theme(legend.position = 'none')
        
        mv2 <- 
          ggplot(mv_data)+
          geom_point(aes(x = run_mean, y = run_var, color = state), size = 3, shape = 16, alpha = 0.5)+
          scale_color_manual(values = c("#1B9E77", "#D95F02"))+
          facet_wrap(~state)+
          ggtitle('Mean-Variance (by state)')+
          ylab('')+
          xlab('Mean (nm)')+
          theme_linedraw(base_size = 18)
        
        cowplot::plot_grid(mv1, mv2, nrow = 1)
      })
    
    #### summarize ####
    events <- eventReactive(input$go_summary, {
      selected_project <- dplyr::filter(projects, name == input$projects_summary)
      
      summary_dribble <- 
        drive_ls(selected_project$id) %>% 
        dplyr::filter(name == "summary")
      
      all_events_dribble <- 
        drive_ls(summary_dribble, pattern = "all-events")
      
      googlesheets4::gs4_auth(email = email, token = drive_token())
      googlesheets4::read_sheet(all_events_dribble)
    })
    
    output$table <- DT::renderDT({
      validate(need(is_tibble(events()), "Please Select a Project"))
      events() %>%
        dplyr::filter(event_user_excluded == FALSE) %>% 
        dplyr::group_by(conditions) %>%
        dplyr::summarize(time_on_avg = mean(time_on_ms, na.rm = TRUE),
                         time_on_se = plotrix::std.error(time_on_ms, na.rm = TRUE),
                         time_on_sd = sd(time_on_ms, na.rm = TRUE),
                         time_on_median = median(time_on_ms, na.rm = TRUE),
                         time_off_avg = mean(time_off_ms, na.rm = TRUE),
                         time_off_se = plotrix::std.error(time_off_ms, na.rm = TRUE),
                         time_off_sd = sd(time_off_ms, na.rm = TRUE),
                         time_off_median = median(time_off_ms, na.rm = TRUE), 
                         displacement_avg = mean(displacement_nm, na.rm = TRUE),
                         displacement_se = plotrix::std.error(displacement_nm, na.rm = TRUE),
                         displacement_sd = sd(displacement_nm, na.rm = TRUE),
                         force_avg = mean(force, na.rm = TRUE),
                         force_se = plotrix::std.error(force, na.rm = TRUE),
                         force_sd = sd(force, na.rm = TRUE),
                         trap_stiffness = mean(trap_stiffness, na.rm = T),
                         myo_stiffness = mean(myo_stiffness, na.rm = T),
                         num_events = n()) %>% 
        mutate(across(where(is.numeric), ~round(., 2))) %>% 
        dplyr::select("Conditions" = conditions, 
                      "Step Size (nm)" = displacement_avg,
                      "SE Step Size" = displacement_se,
                      "Force (pN)" = force_avg,
                      "SE Force" = force_se, 
                      "Avg Time On (ms)" = time_on_avg,
                      "SE Ton" = time_on_se, 
                      'Median Time on (ms)' = time_on_median,
                      "Time Off (ms)" = time_off_avg,
                      "SE Toff" = time_off_se, 
                      "No. Events" = num_events) %>% 
        DT::datatable(
          extensions = 'FixedColumns',
          options = list(
            dom = 't',
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2)
          ))
    })
    
    
    output$step <- renderPlot({
      req(events())
      ggstatsplot::ggbetweenstats(events(),
                                  x = conditions, 
                                  y = displacement_nm,
                                  ylab = "nanometers",
                                  xlab = "",
                                  title = "Displacements",
                                  #ggplot.component = list(scale_color_manual(values = plot_colors)),
                                  centrality.point.args = list(size = 5, color = "grey10"),
                                  ggtheme = theme_cowplot(16))
    }) 
    
    
    output$ton <- renderPlot({
      req(events())
      ggstatsplot::ggbetweenstats(events(),
                                  x = conditions,
                                  y = time_on_ms,
                                  ylab = "milliseconds",
                                  xlab = "",
                                  title = "Attachment Times",
                                  centrality.point.args = list(size = 5, color = "grey10"),
                                  ggtheme = theme_cowplot(16),
                                  type = "nonparametric",
                                  ggstatsplot.layer = F,
                                  ggsignif.args = list(step_increase = 1),
                                  ggplot.component = list(
                                    scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x))),
                                    coord_trans(y = "log10")))
      
    })
    
    output$toff <- renderPlot({
      ggstatsplot::ggbetweenstats(events(),
                                  x = conditions,
                                  y = time_off_ms,
                                  ylab = "milliseconds",
                                  xlab = "",
                                  title = "Time Between Events",
                                  centrality.point.args = list(size = 5, color = "grey10"),
                                  ggtheme = theme_cowplot(16),
                                  type = "nonparametric",
                                  ggstatsplot.layer = F,
                                  ggsignif.args = list(step_increase = 1),
                                  ggplot.component = list(
                                    scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x))),
                                    coord_trans(y = "log10")))
    })
    
    output$force <- renderPlot({
      ggstatsplot::ggbetweenstats(events(),
                                  x = conditions,
                                  y = force,
                                  ylab = "piconewtons",
                                  xlab = "",
                                  title = "Forces",
                                  #ggplot.component = list(scale_color_manual(values = plot_colors)),
                                  centrality.point.args = list(size = 5, color = "grey10"),
                                  ggtheme = theme_cowplot(16))
      
    })
    
    #     rv$ton_ecdf <- time_on_ecdf(event_files_filtered = rv$data$event_files_filtered,
    #                                 plot_colors = plot_colors)
    #     setProgress(0.85, detail = "Time Off ECDF")
    #     rv$toff_ecdf <- time_off_ecdf(event_files_filtered = rv$data$event_files_filtered,
    #                                   plot_colors = plot_colors)
    #     # setProgress(0.85, detail = "Event Frequency")
    #     # rv$ef <- stats_plot_event_frequency(event_file_path = rv$data$event_file_path, 
    #     #                                     factor_order = input$factor_order,
    #     #                                     plot_colors = plot_colors)
    #     setProgress(0.9, detail = "Correlations")
    #     # rv$correlations <- correlations(event_files_filtered = rv$data$event_files_filtered,
    #     #                                 plot_colors = plot_colors)
    #     setProgress(0.95, detail = "Stiffness")
    #     # rv$stiffness <- stiffness(event_files_filtered = rv$data$event_files_filtered,
    #     #                              plot_colors = plot_colors)
    #   })
    # })
    
    
    
  }
  
  
  shinyApp(ui, server)
  
}