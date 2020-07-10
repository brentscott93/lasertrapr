#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard tidyverse
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
                                                                             icon = icon('folder'), 
                                                                             style = 'background-color: #ff41c8'),
                                                mod_folder_manager_ui("folder_manager_ui"),
                                              placement = 'left',
                                              padding = '1px') #dropMenu close
                                      ) #tags$li close
                      ),#header close
      
      dashboardSidebar(
       shinydashboard::sidebarMenu( 
          menuItem(text = 'Home', tabName =  'home', icon = icon('home')),
          menuItem(text = 'Make Observations', tabName = 'obs',  icon = icon('eye')),
          menuItem(text = 'Clean & Process', tabName = 'clean', icon = icon('broom'))
          #menuItem(text = 'Mean-Variance', tabName = 'mv')
        
        )
      ),
      
      dashboardBody(fresh::use_theme(theme_lasertrapr()),
        tabItems(
          tabItem('home', fluidRow(img(src = 'www/lasertrapr-logo.gif'))),
          tabItem('obs', mod_split_obs_ui('split_obs')),
          tabItem('clean', mod_clean_data_ui('clean_data_ui'))
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

