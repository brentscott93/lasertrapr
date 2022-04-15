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
    tabBox(title = '',
               width = 3,
                 tabPanel(
                   title = "Data",
                    h5('Selected Project'),
                    verbatimTextOutput(ns('current_project')),
                    selectInput(ns("gg_type"), "Graph Type", choices = c("Bar", "Box", "Histogram", "Scatter", "Violin")),
              # uiOutput(ns('user_defaults')),
               
               # shinyWidgets::materialSwitch(ns('facet'), 
               #                              'Facet',
               #                              value = FALSE, 
               #                              status = 'primary'),
               # conditionalPanel(condition = "input.facet === true", ns = ns,
               #    uiOutput(ns('facet_options')),             
               # ),
               actionButton(ns('load_data'),
                            'Load Data',
                            icon = icon('rotate'),
                            width = "49%",
                            style = "display: inline-block;"),
               actionButton(ns('export'),
                            'Export',
                            icon = icon('file-export'),
                            width = "49%",
                            style = "display: inline-block;")
           ),
           tabPanel(
             title = "Aes",
             uiOutput(ns("aes"))
           ), 
           tabPanel(
             title = "Scales",
             uiOutput(ns("scales"))
           ),
           tabPanel(
             title = "Facet",
             uiOutput(ns("facet"))
           ), 
           tabPanel(
             title = "Theme",
             uiOutput(ns("theme"))
           )
        ),
       box(
         title = "Plot", 
         width = 9,
          plotOutput(ns("gg"))
      )
  )
}
    
#' figures Server Functions
#'
#' @noRd 
mod_figures_server <- function(input, output, session, f){
    ns <- session$ns
 
    rv <- reactiveValues(all_measured_events = data.frame(),
                         split_conditions = NULL)
    observeEvent(input$load_data, ignoreInit = TRUE, {
      req(f$project$path)
      summary_folder <- file.path(f$project$path, "summary")
      defend_if(!dir.exists(summary_folder), 
                ui = "This project does not have a 'summary' folder. Please Summarize data before making a graph.", 
                type = "error")
      path <- list.files(summary_folder,
                         pattern = "all-measured-events.csv", 
                         full.names = FALSE, 
                         recursive = TRUE)
      dates <- sub("_.*", "", path)
      path <- path[which.max(as.Date(dates))]
      full_path <- list.files(summary_folder, 
                              pattern = path,
                              full.names = TRUE)
      rv$all_measured_events <- fread(full_path)
      split_conditions <- list.files(f$project$path, pattern = "split-conditions.csv", recursive = TRUE, full.names = TRUE)
      if(length(split_conditions) == 0){
        rv$data_summary <- summarize_trap(rv$all_measured_events, by = "conditions")
        rv$split_conditions <- data.frame(conditions = unique(rv$all_measured_events$conditions))
      } else {
        split_conditions <- fread(split_conditions)
        rv$data_summary <- summarize_trap(rv$all_measured_events, by = names(split_conditions))
        rv$split_conditions <- split_conditions
      }
    })
    
    # output$facet_options <- renderUI({
    #   req(conditions())
    #   tagList(
    #     selectInput(ns("facet_var1"), "Facet Variable 1", choices = c("fixed", "free", "free_x", "free_y")),
    #     sliderInput(ns("facet_ncol"), "Number of Columns in Facet", min = 1, max = length(conditions()), value = 1, step = 1),
    #     selectInput(ns("facet_scales"), "Facet Scales", choices = c("fixed", "free", "free_x", "free_y")),
    #     
    #   )
    # })
    
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
    
    
    output$aes <- renderUI({
      validate(need(nrow(rv$split_conditions) > 0, "Select project & upload data to graph"))
      req(input$gg_type)
            
      var <-  if(input$gg_type == "Histogram"){
         selectInput(ns("x"), "X-Axis Variable", choices = c("displacement_nm", "force", "time_on_ms", "time_off_ms"))
         } else {
          choices <- names(rv$all_measured_events)
          tagList(
            selectInput(ns("x"), "X-axis Variable", choices = choices, selected = "conditions"),
            selectInput(ns("y"), "Y-axis Variable", choices = choices, selected = "displacement_nm")
           )
         }

      fill_by <- if(is.null(rv$split_conditions)){
       selectInput(ns("fill_by"), "Fill By", choices = "conditions")
       } else {
        selectInput(ns("fill_by"), "Fill By", choices = names(rv$split_conditions))
      }
       
      factor_order <-  selectInput(ns('factor_order'),
                                   label = 'Factor Order',
                                   multiple = T,
                                   choices = rv$split_conditions[[input$x]])
        
      to_color <- unique(rv$split_conditions[[input$fill_by]])
      
      colorz <- if(length(to_color) == 1){
                  "#002cd3"
            } else if(length(to_color) == 2){
              c("#002cd3", "#d30000")
            } else {
              RColorBrewer::brewer.pal(length(conditions()), 'Set1')
            }
      
      col_picker <- purrr::map2(seq_along(to_color),
                                colorz,
                                ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
                                                                                               label = paste('Color', .x),
                                                                                               value = .y)))
      
      
      #need factor_order
      tagList(var, factor_order, fill_by, col_picker)
     
    })
    
    observe({
      #  req(f$project$path)
      #  req(conditions())
      #  plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
      # if(input$gg_type == "Bar"){
      #   rv$plot <- ggbarplot(
      #     data = step_size_sim,
      #     x =  input$x,
      #     y = input$y,
      #     size = 1,
      #     fill = input$fill,
      #     position = position_dodge(),
      #     add = "mean_se",
      #     add.params = list(size = 1),
      #     error.plot = "upper_errorbar",
      #     order = input$factor_order,
      #     title = input$title,
      #     xlab = input$xlab,
      #     ylab = input$ylab
      #   )+scale_y_continuous(expand = expansion(c(0, 0.1)))
      # 
      # } else if(input$gg_type == "Box"){
      #   rv$plot <-  ggpubr::ggboxplot()
      # } else if(input$gg_type == "Histogram"){
      #   rv$plot <- ggpubr::gghistogram()
      # } else if(input$gg_type == "Scatter"){
      #   rv$plot <- ggpubr::ggscatter()
      # } else if(input$gg_type == "Violin"){
      #   rv$plot <- ggpubr::ggviolin()
      # }
        
    })
    output$gg <- renderPlot({
      validate(need(nrow(rv$split_conditions) > 0, "Upload data to begin"))
      rv$plot
    })
    
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
