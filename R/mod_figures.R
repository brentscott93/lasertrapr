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
    fluidRow(
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
               actionButton(ns('save_plot'),
                            'Save Plot',
                            icon = icon('save'),
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
             shinyWidgets::radioGroupButtons(
               inputId = ns("number_facet_var"),
               label = "Number of Facet Variables",
               choices = c("None", "One", "Two"),
               justified = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-check-square",
                              style = "color: black"),
                 no = tags$i(class = "fa fa-square-o",
                             style = "color: black"))
             ),
             uiOutput(ns("facet"))
           ), 
           tabPanel(
             title = "Theme",
             selectInput(ns("theme"), 
                         "Select a Theme",
                         choices = c("cowplot",
                                     "bw",
                                     "linedraw",
                                     "light",
                                     "light",
                                     "dark",
                                     "minimal",
                                     "classic",
                                     "void")),
             sliderInput(ns("theme_size"), "Base Size", min = 1, max = 30, value = 12, step = 1),
             selectInput(ns("legend_position"),
                         "Legend Position",
                          choices = c("none", "left", "top", "right", "bottom")
             ),
             textInput(ns("legend_title"), "Legend Title", value = ""),
          
             uiOutput(ns("theme"))
           )
        ),
       box(
         title = "Plot", 
         width = 8,
          plotOutput(ns("gg"), height = "auto")),
    
      column(1, 
             box(width = NULL, 
                 title = "Height",
             shinyWidgets::noUiSliderInput(ns("plot_height"),
                                           "",
                                           min = 0, 
                                           max = 650, 
                                           value = 500,
                                           tooltips = FALSE, 
                                           step = 5, 
                                           direction = "rtl", 
                                           orientation = "vertical", 
                                           color = "#444444",
                                           width = "100px", 
                                           height = "600px"))
      )
    ),
    fluidRow(
        column(3),
        column(9,
        box(width = NULL,
            title = "Width",
         shinyWidgets::noUiSliderInput(ns("plot_width"),
                                               "", 
                                               min = 100, 
                                               max = 950, 
                                               value = 900, 
                                               tooltips = FALSE, 
                                               step = 5, 
                                               color = "#444444",
                                               width = "100%" )
        )
      )
    )
  )

}
    
#' figures Server Functions
#'
#' @noRd 
mod_figures_server <- function(input, output, session, f){
    ns <- session$ns
 
    fig <- reactiveValues(all_measured_events = data.frame(),
                          split_conditions = NULL,
                          legend_position = "none",
                          theme_size = 12)
    
    observeEvent(input$load_data, ignoreInit = TRUE, {
      defend_if_null(f$project_input, ui = "Please select a project before plotting", type = "error")
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
        tagList(
         selectInput(ns("x"), "X-Axis Variable", choices = c("displacement_nm", "force", "time_on_ms", "time_off_ms")),
         sliderInput(ns("bins"), "Select Number of Bins", min = 1, max = 100, value = 30)
        )
         } else if(input$gg_type %in% c("Bar", "Box", "Violin")){
          xchoices <- names(fig$split_conditions)
          ychoices <-  c("displacement_nm", "force", "time_on_ms", "time_off_ms")
          tagList(
            selectInput(ns("x"), "X-axis Variable", choices = xchoices, selected = "conditions"),
            selectInput(ns("y"), "Y-axis Variable", choices = ychoices, selected = "displacement_nm")
           )
         } else if(input$gg_type == "Scatter"){
           xchoices <- names(fig$data)
           ychoices <-  names(fig$data)
           tagList(
             selectInput(ns("x"), "X-axis Variable", choices = xchoices, selected = "displacement_nm"),
             selectInput(ns("y"), "Y-axis Variable", choices = ychoices, selected = "time_on_ms")
           )
         }
    })

    output$aes_fill_by <- renderUI({
      req(fig$split_conditions)
      selectInput(ns("fill_by"), "Fill By", choices = c(NULL, names(fig$split_conditions)))
    })
    
    output$aes_factor_order <- renderUI({
      req(input$x %in% names(fig$split_conditions))
      if(input$x %in% names(fig$split_conditions)){
            selectInput(ns('factor_order'),
                        label = paste0('Order X-Axis Variables (', input$x, ")"),
                        multiple = T,
                        choices = fig$split_conditions[[input$x]])
      }
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

    tagList(
      purrr::map2(seq_along(to_color),
                            colorz,
                            ~div(style = 'display: inline-block;', colourpicker::colourInput(ns(paste0('color', .x)),
                                                                                           label = paste('Color', .x),
                                                                                           value = .y)
                                 )
                                )
                              )
    })
 

    output$xy_labels <- renderUI({
      req(input$x)
      req(input$y)
     
      tagList(
        textInput(ns("xlab"), "X Label", value = input$x),
        textInput(ns("ylab"), "Y Label", value = input$y),
        checkboxInput(ns("scale_y_log"), "Log Y-axis?")
       )

    })
    
    output$facet <- renderUI({
        req(fig$data)
        req(fig$split_conditions)
        req(input$number_facet_var)
        if(input$number_facet_var != "None"){
          if(input$number_facet_var == "One"){
            tagList(
            selectInput(ns("facet_by1"), "Facet By", choices = c(NULL, names(fig$split_conditions))),
            sliderInput(ns("facet_nrow"), "Number of Facet Rows", min = 1, max(nrow(fig$split_conditions)), value = 2, step = 1)
            )
          } else {
            tagList(
              purrr::map(seq_len(2), ~ selectInput(ns(paste0("facet_by", .x)), 
                                                   paste("Facet By", .x), 
                                                   choices = names(fig$split_conditions),
                                                   selected = names(fig$split_conditions)[[.x]])),
              sliderInput(ns("facet_nrow"), "Number of Facet Rows", min = 1, max(nrow(fig$split_conditions)), value = 1, step = 1)
            )
          }
        }
    })

    output$theme <- renderUI({
      req(input$fill_by %in% names(fig$split_conditions))
      if(input$x != input$fill_by){
      selectInput(ns('legend_order'),
                  label = paste0('Legend Order (', input$fill_by, ")"),
                  multiple = T,
                  choices = fig$split_conditions[[input$fill_by]])
    }
    })
    
#### FIG options ####
    observe({
      if(!is.null(input$legend_position)){
        fig$legend_position <- input$legend_position
      }
      
      if(!is.null(input$theme_size)){
        fig$theme_size <- as.numeric(input$theme_size)
      }
      
    })
#### MAKE PLOT ####
    observe({
     # browser()
      req(nrow(fig$split_conditions) > 0)
      req(input$gg_type)
      req(input$x)
      req(input$fill_by)
      to_color <- unique(fig$split_conditions[[input$fill_by]]) 
      req(input$color1)
      fig$plot_colors <- purrr::map_chr(seq_along(to_color), ~input[[paste0("color", .x)]])
      
      plot_data <- fig$data
      
      if(input$x %in% names(fig$split_conditions)){
        if(!is.null(input$factor_order)){
         plot_data[[input$x]] <-  factor(plot_data[[input$x]], levels = input$factor_order)
        }
      }
    
      
      if(input$x != input$fill_by){
        if(!is.null(input$legend_order)){
        plot_data[[input$fill_by]] <- factor(plot_data[[input$fill_by]], levels = input$legend_order)
       }
      }
    
      if(input$gg_type == "Bar"){
      
         plot_data_sum <- plot_data[, .(y = mean(base::get(input$y), na.rm = TRUE),
                                   y_se = sd(base::get(input$y), na.rm = TRUE)/sqrt(.N)),
                                   by = names(fig$split_conditions)]
         
      
        fig$plot <- 
          ggplot()+
          geom_errorbar(data = plot_data_sum,
                        aes(x = base::get(input$x),
                            ymin = y-y_se,
                            ymax = y+y_se,
                            group = base::get(input$fill_by)),
                        width = 0.25,
                        size=1,
                        position = position_dodge(width=0.9),
                        show.legend = FALSE)+
             geom_col(data = plot_data_sum,
                      aes(x = base::get(input$x),
                          y = y,
                          fill = base::get(input$fill_by)),
                      color = "black",
                      position = position_dodge(), 
                      size=1)+
          scale_y_continuous(expand = expansion(c(0, 0.1)))


      } else if(input$gg_type == "Box"){
    
        
          fig$plot <- 
            ggplot()+
            geom_boxplot(data = plot_data,
                     aes(x =  base::get(input$x),
                         y = base::get(input$y),
                         fill = base::get(input$fill_by)),
                     color = "black",
                     position = position_dodge(), 
                     size=1)
          
      } else if(input$gg_type == "Histogram"){
        
    
        fig$plot <- 
          ggplot()+
          geom_histogram(data = plot_data,
                       aes(x = base::get(input$x),
                           y = stat(density),
                           fill = base::get(input$fill_by)),
                       color = "black",
                       position = position_dodge(), 
                       size=1,
                       bins = as.numeric(input$bins))+
          scale_y_continuous(expand = expansion(c(0, 0.1)))
        
      } else if(input$gg_type == "Scatter"){
      
        
        fig$plot <- 
          ggplot()+
          geom_point(data = plot_data,
                         aes(x = base::get(input$x),
                             y = base::get(input$y),
                             color = base::get(input$fill_by)),
                         shape = 16,
                       alpha = 0.5)
        
   
      } else if(input$gg_type == "Violin"){
        
  
        fig$plot <- 
          ggplot(data = plot_data,
                  aes(x = base::get(input$x),
                      y = base::get(input$y),
                      fill = base::get(input$fill_by)))+
          geom_violin(color = "black",
                     position = position_dodge(), 
                     size=1)+
          geom_boxplot(width=0.1, color="black")
        
      }
        
    })
    
  

  
#observe({
  output$gg <- renderPlot({
      validate(need(fig$data, "Upload data to begin"))
      validate(need(fig$plot, "Activate 'Aes' tab to display graph."))
      
     if(input$gg_type == "Histogram"){
        if(!is.numeric(fig$data[[input$x]])){
          validate(need(is.numeric(fig$data[[input$x]]), "Please select a continuous x-variable for the histogram"))
        }
     }

      if(input$number_facet_var == "None"){
        gg_final <- fig$plot
      } else if(input$number_facet_var == "One"){
        req(input$facet_by1)
        gg_final <-  
          fig$plot +
          facet_wrap(as.formula(
                      paste("~", 
                            input[["facet_by1"]])),
                     nrow = as.numeric(input$facet_nrow))
      } else if(input$number_facet_var == "Two") {
        req(input$facet_by2)
        gg_final <-  
          fig$plot +
          facet_wrap(as.formula(
                      paste(input[["facet_by1"]],
                            "~",
                            input[["facet_by2"]])),
                     nrow = as.numeric(input$facet_nrow))
      }
      
      gg_final <-  
        gg_final+
        ggtitle(input$title)+
        xlab(input$xlab)+
        ylab(input$ylab)+
        theme_cowplot()
      
      if(input$gg_type == "Scatter"){
        gg_final <- gg_final+scale_color_manual(input$legend_title, values = fig$plot_colors)
      } else {
        gg_final <- gg_final+scale_fill_manual(input$legend_title, values = fig$plot_colors)
      }
      
      gg_final <- 
        if(is.null(input$scale_y_log)){
            gg_final
        } else {
          if(input$gg_type %in% c("Histogram", "Bar")){
            if(input$scale_y_log){
              gg_final+scale_y_log10(expand = expansion(c(0, 0.1)))
            } else {
              gg_final+scale_y_continuous(expand = expansion(c(0, 0.1)))
            }
          } else {
            if(input$scale_y_log){
              gg_final+scale_y_log10()
            } else {
              gg_final+scale_y_continuous()
            }
          }
        }

     gg_final <-
       if(is.null(input$theme)){
        gg_final
      } else {
        gg_final + eval(parse(text = paste0("theme_", 
                                            input$theme, 
                                            "(", 
                                            fig$theme_size,
                                            ")"
                                            )
                              )
                        )
      }
  
     gg_final <- 
       if(input$theme == "cowplot"){
       gg_final+
         theme(
           legend.position = fig$legend_position,
           strip.background = element_blank()
       )
     } else {
       gg_final+
       theme(
         legend.position = fig$legend_position
       )
     }
 
     fig$gg_final <- gg_final
     
     gg_final
     
    }, width = function()input$plot_width, 
       height = function()input$plot_height
    )

  
  observeEvent(input$save_plot, {
    
    filename <- paste0(gsub(":", "-", sub(" ", "_", Sys.time())), "_plot")
    fig$size_ratio <- input$plot_width/input$plot_height
    showModal(
      modalDialog(
        title = "Save Plot As...",
        textInput(ns("save_as_file_name"),
                  "Filename",
                  value = filename),
        shinyWidgets::radioGroupButtons(
          inputId = ns("save_as_file_type"),
          label = "File Type",
          choices = c("jpg", "png", "pdf", "rds"),
          justified = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: black"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: black"))
        ),
        
        conditionalPanel("input.save_as_file_type != 'rds'", ns = ns,
                         numericInput(ns("save_width"), 
                                      "Width of Plot (inches, aspect ratio preserved)",
                                      value = "8")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_as_modal_ok"), "OK")
        )
      )
    )
  })
  
  
  
  observeEvent(input$save_as_modal_ok, {
    
    target_dir <-  file.path(f$project$path, "summary", "figures")
    if(!dir.exists(target_dir)){
      dir.create(target_dir)
    }
    filename <- paste0(input$save_as_file_name, ".", input$save_as_file_type)
    if(input$save_as_file_type == "rds"){
      saveRDS(fig$gg_final, 
              file = file.path(target_dir, filename))
    } else {
      
      
      ggsave(filename = file.path(target_dir, filename),
             plot = fig$gg_final,
             height = input$save_width/fig$size_ratio,
             width = input$save_width, 
             units = "in",
             bg = "white")
    }
    showNotification(paste("Plot saved as:", filename), type = "message")
    removeModal()
  })
    
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
