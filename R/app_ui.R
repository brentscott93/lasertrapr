#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "lasertrapr",
                                      tags$li(class = "dropdown",
                                              shinyWidgets::dropMenu(
                                                shinyWidgets::dropdownButton('Files',
                                                                             icon = icon('folder')),
                                                mod_folder_manager_ui("folder_manager_ui"),
                                              placement = 'left',
                                              theme = 'translucent',
                                              padding = '1px') #dropMenu close
                                      ) #tags$li close
                      ),#header close
      
      dashboardSidebar(
       shinydashboard::sidebarMenu(id = "sidemenu",
          menuItem(text = 'Home', tabName =  'home', icon = icon('home')),
          menuItem(text = 'Upload Data', tabName = 'obs',  icon = icon('play-circle')),
          menuItem(text = 'Clean & Process', tabName = 'clean', icon = icon('broom')),
          menuItem(text = 'Analyzers', icon = icon('microscope'),
           menuSubItem(text = 'Single Molecule - 1 QPD', tabName = 'hm_model'),
           menuSubItem(text = 'Covariance', tabName = 'covariance'),
           menuSubItem(text = 'Mini-Ensemble', tabName = 'mini_ensemble'),
           menuSubItem(text = 'Isometric Force Clamp', tabName = 'isometric_force_clamp')),
          menuItem(text = 'Summarize', tabName = 'summarize', icon = icon('sort')),
          menuItem(text = 'Ensemble Average', icon = icon('align-left'),
           menuSubItem(text = 'Myosin', tabName = 'ensemble_average_myosin'),
           menuSubItem(text = 'Force Ramp', tabName = 'ensemble_average_force_ramp')),
          menuItem(text = 'Figures', tabName = 'figures', icon = icon('chart-bar'))
          #menuItem(text = 'Mean-Variance', tabName = 'mv')
        
        )
      ),
      
      dashboardBody(fresh::use_theme(theme_lasertrapr()),
                    
        tabItems(
          tabItem('home', fluidRow(HTML("<p align='center'>
                                     <img src='www/lasertrapr-logo.gif'>
                                     </p> "))),
          tabItem('obs', mod_split_obs_ui('split_obs')),
          tabItem('clean', mod_clean_data_ui('clean_data_ui')),
          tabItem('hm_model', mod_hm_model_ui("hm_model_ui")),
          tabItem('covariance', mod_covariance_ui("covariance_1")),
          tabItem('mini_ensemble', mod_mini_ensemble_ui("mini_ensemble")),
          tabItem('ensemble_average_myosin', mod_ensemble_average_myosin_ui("ensemble_average_myosin")),
          tabItem('ensemble_average_force_ramp', mod_ensemble_average_force_ramp_ui("ensemble_average_force_ramp")),
          tabItem('isometric_force_clamp', mod_isometric_force_clamp_ui("isometric_force_clamp")),
          tabItem('summarize', mod_summarize_ui("summarize")),
          tabItem('figures', mod_figures_ui("figures"))
         
        #tabItem(tabName = 'mv', mod_mv_ui('mv'))
        )#tab itens close
      )#body close
     )#dashboard page close
  )#taglist close
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'lasertrapr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

