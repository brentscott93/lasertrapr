#' figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_figures_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(3,
           box(title = 'Controls',
               width = NULL,
               h4('Selected Project'),
               verbatimTextOutput(ns('current_project')),
               selectInput(ns("gg_type"), "Graph Type", choices = c("Bar", "Box", "Histogram", "Scatter", "Violin")),
               uiOutput(ns('user_defaults')),
               
               shinyWidgets::materialSwitch(ns('facet'), 
                                            'Facet',
                                            value = FALSE, 
                                            status = 'primary'),
               conditionalPanel(condition = "input.facet === true", ns = ns,
                  uiOutput(ns('facet_options')),             
               ),
               actionButton(ns('go'),
                            'Summarize',
                            icon = icon('calculator'),
                            width = "49%",
                            style = "display: inline-block;"),
               actionButton(ns('export'),
                            'Export',
                            icon = icon('file-export'),
                            width = "49%",
                            style = "display: inline-block;")
           )
    ),
    column(9,
           box(
             title = "Plot", 
             width = NULL,
             plotOutput("gg")
           )
          )
 
  )
}
    
#' figures Server Functions
#'
#' @noRd 
mod_figures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    rv <- reactiveValues(measured_events = data.frame())
    observeEvent(f$project$name, {
      req(f$project$path)
      path <- list.files(f$project$path,
                         pattern = "all-measured-events.csv", 
                         full.names = TRUE, 
                         recursive = TRUE)
      dates <- sub("_.*", "", path)
      path <- path[which.max(as.Date(dates))]
      rv$all_measured_events <- fread(path)
      summary_options <- list.files(f$project_path, pattern = "summary-options.csv", recursive = TRUE, full.names = TRUE)
      if(length(summary_options) == 0){
        rv$data_summary <- summarize_trap(rv$all_measured_events, by = "conditions")
      } else {
        summary_options <- fread(summary_options)
        rv$data_summary <- summarize_trap(rv$all_measured_events, by = names(summary_options))
        rv$summary_options <- summary_options
      }
    })
    
    output$facet_options <- renderUI({
      req(conditions())
      tagList(
        selectInput(ns("facet_var1"), "Facet Variable 1", choices = c("fixed", "free", "free_x", "free_y")),
        sliderInput(ns("facet_ncol", "Number of Columns in Facet", min = 1, max = length(conditions()), step = 1)),
        selectInput(ns("facet_scales"), "Facet Scales", choices = c("fixed", "free", "free_x", "free_y")),
        
      )
    })
    
    output$current_project <- renderText({
      validate(need(f$project$name, 'Select a project'))
      f$project$name
    })
    
    conditions <- reactive({
      req(f$project$path)
      list_dir(f$project$path) %>%
        dplyr::filter(str_detect(name, "summary", negate = TRUE)) %>% 
        dplyr::pull(name)
    })
    
    colorz <- reactive({
      if(length(conditions()) == 1){
        "#002cd3"
      } else if(length(conditions()) == 2){
        c("#002cd3", "#d30000")
      } else {
        RColorBrewer::brewer.pal(length(conditions()), 'Set1')
      }
    })
    
    output$user_defaults <- renderUI({
      req(conditions())
      if(length(conditions()) >= 2){
        tagList(
          
          selectInput(ns('factor_order'),
                      label = 'Factor Order',
                      multiple = T,
                      choices = conditions()),
          purrr::map2(seq_along(conditions()),
                      colorz(),
                      ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
                                                                                     label = paste('Color', .x),
                                                                                     value = .y)))
        )
      } else {
        div(style = 'display:inline-block', colourpicker::colourInput(ns('color1'),
                                                                      label = 'Color 1',
                                                                      value = colorz()))
      }
    })
    
    output$xy_var <- renderUI({
      req(input$gg_type)
      if(input$gg_type == "Histogram"){
         tagList(
       selectInput(ns("x"), "X-Axis Variable", choices = c("displacement_nm", "force", "time_on_ms", "time_off_ms"))
         )
      } else {
        choices <- names(rv$all_measured_events)
        tagList(
          selectInput(ns("x"), "X-axis Variable", choices = choices, selected = "conditions"),
          selectInput(ns("y"), "Y-axis Variable", choices = choices, selected = "displacement_nm")
        )
      }
    })
    
    observe({
      c("Bar", "Box", "Histogram", "Scatter", "Violin")
      if(input$gg_type == "Bar"){
        rv$plot <- ggpubr::ggbarplot()
        
      } else if(input$gg_type == "Box"){
        rv$plot <-  ggpubr::ggboxplot()
      } else if(input$gg_type == "Histogram"){
        rv$plot <- ggpubr::gghistogram()
      } else if(input$gg_type == "Scatter"){
        rv$plot <- ggpubr::ggscatter()
      } else if(input$gg_type == "Violin"){
        rv$plot <- ggpubr::ggviolin()
      }
        
    })
    output$gg <- renderPlot({
      
    })
    
  })
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
