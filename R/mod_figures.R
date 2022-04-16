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
             uiOutput(ns("aes_var")),
             uiOutput(ns("aes_factor_order")),
             uiOutput(ns("aes_fill_by")),
             uiOutput(ns("aes_colors")),
             
             
             
           ), 
           tabPanel(
             title = "Scales",
            textInput(ns("title"), "Title", value = ""),
            uiOutput(ns("xy_labels"))
           
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
 
    fig <- reactiveValues(all_measured_events = data.frame(),
                          split_conditions = NULL)
    
    observeEvent(input$load_data, ignoreInit = TRUE, {
      req(f$project_input)
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
      fig$data <- fread(full_path)
      split_conditions <- list.files(f$project$path, pattern = "split-conditions.csv", recursive = TRUE, full.names = TRUE)
      if(length(split_conditions) == 0){
        fig$data_summary <- summarize_trap(fig$data, by = "conditions")
        fig$split_conditions <- data.frame(conditions = fig$data_summary$conditions)
      } else {
        split_conditions <- fread(split_conditions)
        fig$data_summary <- summarize_trap(fig$data, by = names(split_conditions))
        fig$split_conditions <- split_conditions
      }
      showNotification("Data Loaded.", type = "message")
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
    
    
    output$aes_var <- renderUI({
      validate(need(nrow(fig$split_conditions) > 0, "Select project & upload data to graph"))
      req(input$gg_type)
            
      if(input$gg_type == "Histogram"){
         selectInput(ns("x"), "X-Axis Variable", choices = c("displacement_nm", "force", "time_on_ms", "time_off_ms"))
         } else if(input$gg_type == "Bar") {
          xchoices <- names(fig$split_conditions)
          ychoices <-  c("displacement_nm", "force", "time_on_ms", "time_off_ms")
          tagList(
            selectInput(ns("x"), "X-axis Variable", choices = xchoices, selected = "conditions"),
            selectInput(ns("y"), "Y-axis Variable", choices = ychoices, selected = "displacement_nm")
           )
         }
    })

    output$aes_fill_by <- renderUI({
      req(fig$split_conditions)
      selectInput(ns("fill_by"), "Fill By", choices = c(NULL, names(fig$split_conditions)))
    })
    
    output$aes_factor_order <- renderUI({
          req(input$x %in%names(fig$split_conditions))
            selectInput(ns('factor_order'),
                        label = 'Factor Order',
                        multiple = T,
                        choices = fig$split_conditions[[input$x]])
    })
    
    
    output$aes_colors <- renderUI({
      req(input$x)
      req(input$fill_by)
      to_color <- unique(fig$split_conditions[[input$fill_by]]) 
      
      colorz <-  if(length(to_color) == 1){
                  "#002cd3"
                   } else if(length(to_color) == 2){
                    c("#002cd3", "#d30000")
                  } else {
                     RColorBrewer::brewer.pal(length(to_color), 'Set1')
                  }

    
      purrr::map2(seq_along(to_color),
                            colorz,
                            ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
                                                                                           label = paste('Color', .x),
                                                                                           value = .y
                                                                                           )
                            )
      )
    })
 

    output$xy_labels <- renderUI({
      req(input$x)
      req(input$y)
      if(input$gg_type == "Bar"){
      tagList(
        textInput(ns("xlab"), "X Label", value = input$x),
        textInput(ns("ylab"), "Y Label", value = input$y),
        checkboxInput(ns("scale_y_log"), "Log Y-axis?")
       )
      } else {
        tagList(
          textInput(ns("xlab"), "X Label", value = input$x),
          textInput(ns("ylab"), "Y Label", value = input$y)
        )
      }
    })
    
    observe({
     # browser()
      req(nrow(fig$split_conditions) > 0)
      req(input$gg_type)
      req(input$x)
      req(input$fill_by)
      to_color <- unique(fig$split_conditions[[input$fill_by]]) 
      req(input$color1)
      fig$plot_colors <- purrr::map_chr(seq_along(to_color), ~input[[paste0("color", .x)]])
      
      if(input$gg_type == "Bar"){
      
         plot_data <- fig$data[, .(y = mean(base::get(input$y), na.rm = TRUE),
                                   y_se = sd(base::get(input$y), na.rm = TRUE)/sqrt(.N)),
                                   by = names(fig$split_conditions)]
         
         plot_data$x <- plot_data[[input$x]]

         if(!is.null(input$factor_order)){
         plot_data$x <- factor(plot_data$x, levels = input$factor_order)
         }
         
        fig$plot <- 
          ggplot()+
          geom_errorbar(data = plot_data,
                        aes(x = x,
                            ymin = y-y_se,
                            ymax = y+y_se,
                            group =  base::get(input$fill_by)),
                        width = 0.25,
                        size=1,
                        position = position_dodge(width=0.9),
                        show.legend = FALSE)+
             geom_col(data = plot_data,
                      aes(x = x,
                          y = y,
                          fill = base::get(input$fill_by)),
                      color = "black",
                      position = position_dodge(), 
                      size=1,
                      show.legend = FALSE)+
          scale_y_continuous(expand = expansion(c(0, 0.1)))
          
    
        #   ggpubr::ggbarplot(
        #   data = plot_data,
        #   x =  x,
        #   y = input$y,
        #   size = 1,
        #   fill = input$fill_by,
        #   palette = fig$plot_colors,
        #   #position = position_dodge(),
        #   add = "mean_se",
        #   add.params = list(size = 1),
        #   error.plot = "upper_errorbar",
        #   order = input$factor_order,
        #   title = input$title,
        #   xlab = input$xlab,
        #   ylab = input$ylab
        # )+ggplot2::scale_y_continuous(expand = expansion(c(0, 0.1)))

      } else if(input$gg_type == "Box"){
    #    rv$plot <-  ggpubr::ggboxplot()
      } else if(input$gg_type == "Histogram"){
    #   rv$plot <- ggpubr::gghistogram()
      } else if(input$gg_type == "Scatter"){
     #   rv$plot <- ggpubr::ggscatter()
      } else if(input$gg_type == "Violin"){
     #   rv$plot <- ggpubr::ggviolin()
      }
        
    })
    output$gg <- renderPlot({
      validate(need(fig$data, "Upload data to begin"))
      validate(need(fig$plot, "Activate 'Aes' tab to display graph."))
      
      gg_final <-  
        fig$plot+
        ggtitle(input$title)+
        xlab(input$xlab)+
        ylab(input$ylab)+
        scale_fill_manual(values = fig$plot_colors)+
        theme_cowplot()
      
      if(is.null(input$scale_y_log)){
        gg_final
      } else {
        if(input$scale_y_log){
          gg_final+scale_y_log10(expand = expansion(c(0, 0.1)))
        } else {
          gg_final
        }
      }
      
    })
    
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
