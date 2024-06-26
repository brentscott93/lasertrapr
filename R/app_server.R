#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  lasertrapr_folder <- file.path(path.expand('~'), 'lasertrapr') 
  
  if(file.exists(lasertrapr_folder) == FALSE){
    dir.create(lasertrapr_folder)
    showModal(modalDialog( title = "Welcome!",
                           paste0('All your data will be saved to: ', lasertrapr_folder)
                           
    ))
  }
  
  #reactive values to store currently selected folders
  f <- reactiveValues(project = data.frame(name = NA, path = 'please select'),
                      conditions = data.frame(name = NA, path = 'please select'),
                      #date = tibble(name = NA, path = 'please select'),
                      new_obs = 0,
                      new_obs_refresh_graph = 0,
                      new_obs_from_split= 0)
  # observe({golem::print_dev(str(f$date))})
  # 
  # observe({golem::print_dev(str(f$project))})
  # 
  # observe({golem::print_dev(str(f$conditions))})
  
  #### CALL MODULES ####
  #callModule(mod_mv_server, "mv")
  callModule(mod_folder_manager_server, "folder_manager_ui", lasertrapr_folder = lasertrapr_folder, f = f)
  
  callModule(mod_split_obs_server, "split_obs", f = f)
  
  callModule(mod_clean_data_server, "clean_data_ui", f = f)
  
  callModule(mod_hm_model_server, "hm_model_ui", f = f)

  callModule(mod_covariance_server, "covariance_1", f = f)
  
  callModule(mod_mini_ensemble_server, "mini_ensemble", f = f)

  callModule(mod_isometric_force_clamp_server, "isometric_force_clamp", f = f)

  callModule(mod_ensemble_average_server, "ensemble_average", f = f, input_sidemenu = reactive(input$sidemenu))
  
  callModule(mod_summarize_server, "summarize", f = f)
  
  callModule(mod_figures_server, "figures", f = f)
  
  
  
}
