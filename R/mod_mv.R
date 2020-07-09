#' mv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinydashboard
mod_mv_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(
    #   column(12, 
    #          box(width = NULL, title = 'Status',
    #          verbatimTextOutput(ns('values')),
    #          )
    #   ) #col close
    # ), #row close
    
    fluidRow(
      column(4, 
             box(width = NULL, 
                 title = 'Controls',
                 
                 uiOutput(ns('select_obs'), style = 'display:inline-block;vertical-align:top;'),
                 div(style = 'display:inline-block;vertical-align:top;margin-top:25px;',
                    actionButton(ns('set_windows'),
                                  'Set Windows'),
                    actionButton(ns('calculate_mv'),
                                    'Calculate MV')
                 ),
                    uiOutput(ns('plot_2_show')),
                 h5('Select area on plot to set baseline/event population'),
                 plotOutput(ns('mv'), brush = ns('mv_brush')),
                 div(style='display:inline-block',
                     actionButton(ns('set_baseline'), 'Set Baseline', width = '240px'),
                     actionButton(ns('set_event'), 'Set Event', width = '240px')
                 ),
                 br(),
                 actionButton(ns('measure'), 'Measure Events', width = '100%',  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 #actionButton(ns('measure2'), 'Measure Events', width = '100%')
             ) # box close
      ),#col close
      column(4, 
            box(width = NULL,
            title = 'Step Size',
              verbatimTextOutput(ns('step_size')),
              plotOutput(ns('baseline_histo')),#  %>% shinycssloaders::withSpinner(type = 8),
              plotOutput(ns('event_histo')),
            ) #box close
      ),#column close
      column(4, 
             box(width = NULL,
                 title = 'Time On & Event Count',
                     plotOutput(ns('time_on_plot')),# %>% shinycssloaders::withSpinner(type = 8),
                     verbatimTextOutput(ns('time_on_summary'))
                      
             )
      )
      
      ) #row close
 
  ) #taglist close
}
    
#' mv Server Function
#'
#' @noRd 
mod_mv_server <- function(input, output, session, user){
  
  ns <- session$ns
  rv <- reactiveValues()
  
  output$select_obs <- renderUI({

        selectInput(ns('obs'), 
                label = 'Select Observation',
                choices = 1:3)
    
  })
  
  
  observeEvent(input$set_windows, {
    showModal(modalDialog(
      title = "Set Windows",
      sliderInput(ns('n_windows'),
                  'Choose Number of Windows',
                  min = 6,
                  max = 12,
                  value = 8),
      h4('Enter windows width (in milliseconds)'),
      uiOutput(ns('windows')),
      
      easyClose = TRUE,
      footer = tagList(actionButton(ns('confirm_windows'), 'Confirm'),
                       modalButton('Cancel'))
    ))
  })
  
  
  observeEvent(input$confirm_windows, {
    
    win <-  map(window_id(), ~input[[.]])
    
      if(anyNA(as.numeric(unlist(win)))){
        shiny::showNotification('Missing window width values', type = 'error')
      } else {
        rv$ww <- win
        removeModal()
      }
      
  })
  
  window_id <-  reactive({
   n <- seq_len(input$n_windows)
   paste0('window', n)
  })
  
  output$windows <- renderUI({
   
    imap(window_id(), ~ div(style = 'display:inline-block', textInput(ns(.x), paste0('Window ', .y), placeholder = 'milliseconds', width = '125px')))
    
  })
  
  output$values <- renderPrint({
    win_vals <- as.numeric(unlist(rv$ww))
    validate(need(!is_empty(win_vals), 'Windows not set'))
    cat('Windows: ', win_vals, ' | Step Size: ', step_size$step, 'nm | Number of Events: ')
  })
  
  
  output$plot_2_show <- renderUI({
    win_vals <- as.numeric(unlist(rv$ww))
    shinyWidgets::sliderTextInput(ns('plot_2_show'),
                   'Select MV plot to show',
               choices = windows,
               grid = T,
               width = '100%')
  
  })
  
  output$mv <- renderPlot({
    req(not_null(current_mv()))
    ggplot(data = current_mv())+
      geom_hex(aes(x = mean, y = var), bins = 90)+
      theme_classic()
  })
  
  # observe({
  # golem::print_dev(input$mv_brush)
  # })
  

  # Read Data ---------------------------------------------------------------
  
   
  
  
  # Calculate Windows -------------------------------------------------------
  
  #start mv
  # mv_df <- map_df(ww, function(x) tibble(mean = RcppRoll::roll_mean(obs, n = x, align = 'left', fill = NULL),
  #                                        var = RcppRoll::roll_var(obs, n = x, align = 'left', fill = NULL),
  #                                        ww = x))
  
  #sim_dat <- readRDS('~/rstats/play/mv_df.rds')

  #windows <- unique(sim_dat$ww)

   current_mv <- reactive({
     req(not_null(user$data))
     filter(user$data, near(ww, input$plot_2_show))
   })


# Step --------------------------------------------------------------------

  
  baseline_pop <- eventReactive(input$set_baseline, {
    showNotification('Baseline Population Set', type = 'message')
    input$mv_brush
  })
  
  event_pop <- eventReactive(input$set_event, {
    showNotification('Event Population Set', type = 'message')
    input$mv_brush
  })
  
  observe({golem::print_dev(baseline_pop())})
  
  observe({golem::print_dev(event_pop())})
  
  step_size <- reactiveValues(done = 0)
   observeEvent(input$measure, {
     
    #baseline data and fit to density fit
    baseline <- filter(current_mv(), between(mean, baseline_pop()$xmin, baseline_pop()$xmax) & between(var, baseline_pop()$ymin, baseline_pop()$ymax))
    baseline_fit <- MASS::fitdistr(baseline$mean, 'normal')
    
    #event data and fit to density fit
    event <- filter(current_mv(), between(mean, event_pop()$xmin, event_pop()$xmax) & between(var, event_pop()$ymin, event_pop()$ymax))
    event_fit <- MASS::fitdistr(event$mean, 'normal')
    
    #return values to reactive list
    step_size$step <- event_fit$estimate[1] - baseline_fit$estimate[1]
    step_size$baseline <- baseline
    step_size$baseline_fit <- baseline_fit
    step_size$event <- event
    step_size$event_fit <- event_fit
    step_size$done <- step_size$done + 1
   
    showNotification('Step Size Calculated')
  })
    
  output$step_size <- renderPrint({
    validate(need(not_null(step_size$step), 'Events not measured'))
     cat('Difference in Event & Baseline pop. = ', step_size$step, 'nm')
   })
   
  output$baseline_histo <- renderPlot({
    req(not_null(step_size$baseline), not_null(step_size$baseline_fit))
    hist(step_size$baseline$mean, pch=20, breaks=25, prob=T, main="Baseline Population", xlab = 'Displacement (nm)')
    curve(dnorm(x, step_size$baseline_fit$estimate[1], step_size$baseline_fit$estimate[2]), col="red", lwd=2, add=T)
    
  })
  
  output$event_histo <- renderPlot({
    req(not_null(step_size$event), not_null(step_size$event_fit))
    hist(step_size$event$mean, pch=20, breaks=25, prob=T, main="Event Population", xlab = 'Displacement (nm)')
    curve(dnorm(x, step_size$event_fit$estimate[1], step_size$event_fit$estimate[2]), col="red", lwd=2, add=T)
  })
  
  

# Time On -----------------------------------------------------------------
  #time_on <- reactiveValues()
  
 time_on <-  eventReactive( input$measure, {
    #browser()
    withProgress(message = 'Calculating on times...', {
    mv_df <- split(sim_dat, sim_dat$ww)
    setProgress(value = 0.25)
    mv_gg <- map(mv_df, function(x) ggplot(x)+geom_hex(aes(mean, var), bins = 90))
    setProgress(value = 0.5)
    gg_builds <- map(mv_gg, ggplot_build)
    setProgress(value = 0.75)
    gg_builds_data <- map(gg_builds, function(x) x[["data"]][[1]])
    
    gg_builds_data_filter <- map(gg_builds_data, ~filter(.x, between(x, event_pop()$xmin, event_pop()$xmax) & between(y, event_pop()$ymin, event_pop()$ymax)))
    
    roi_counts <- map_dbl(gg_builds_data_filter, function(x) sum(x$count))
  
    #change ww
    
   # roi_min <-which.min(roi_counts)
   # counts <- roi_counts[1:roi_min]
    ww <- as.numeric(names(mv_df))
    time_on_df <- data.frame(x = ww,
                        y = roi_counts)
    
   
    
    setProgress(value = 0.8)
    time_on_fit <- nls(y ~ ton * N * exp((1-x)/ton), data = time_on_df, start = list(ton = 400,
                                                                       N = 200))
  
    line <- tibble(y = predict(time_on_fit, newdata = data.frame(x = 0:max(ww))), 
                   x = 0:max(ww))
    
    setProgress(value = 1)
    # time_on$data <- time_on_df
    # time_on$line <- line
    # time_on$fit <- time_on_fit
    # 
    #golem::print_dev(summary(time_on_fit))
    list(data = time_on_df,
         line = line,
         fit = time_on_fit)
         
    })
    
  })
 
  output$time_on_plot <- renderPlot({
    req(not_null(time_on()$line))
    ggplot()+
      geom_point(data = time_on()$data, aes(x, y))+
      geom_line(data = time_on()$line, aes(x, y))+
      xlab('Window Width (datapoints)')+
      ylab('Counts')+
      theme_classic()  
  })
  
  output$time_on_summary <- renderPrint({
    summary(time_on()$fit)
  })

  
  
  
    
 
  
}
  
  
    
## To be copied in the UI
# mod_mv_ui("mv")
    
## To be copied in the server
# callModule(mod_mv_server, "mv")
 
#' mv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mv_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mv Server Function
#'
#' @noRd 
mod_mv_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_mv_ui("mv_ui_1")
    
## To be copied in the server
# callModule(mod_mv_server, "mv_ui_1")
 
