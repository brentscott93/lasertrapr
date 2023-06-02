#' Move selected data to new obs
#' @param trap_selected_date current selected date folder
#' @param trap_selected_obs current selected observation
#' @param trim_from begin index to delete
#' @param trim_to end index to delete
#' @param f the reactiveValues list 'f' with all user selected folder info
#' @param trap_obs obs folder numbers
move_obs <- function(trap_selected_date, trap_selected_obs, trim_from, trim_to, f, trap_obs, hz){
  withProgress(message = 'Moving Data', value = 0, max = 1, min = 0, {
    
    new_obs <- trap_obs + 1
    if(new_obs < 10){
      new_folder <- paste0("obs-", 0, new_obs)
    } else {
      new_folder <- paste0("obs-", new_obs)
    }
    
    incProgress(amount = .25, detail = "Creating new folder")
    new_folder_path <- paste0(trap_selected_date, "/", new_folder)
    dir.create(path = new_folder_path)
    setProgress(0.4)
    
    path <- list_files(trap_selected_obs, pattern = 'trap-data.csv')
    data <- data.table::fread(path$path, sep = ",")
    
    options_path <- list_files(trap_selected_obs, pattern = 'options.csv')
    options_data <- data.table::fread(options_path$path, sep = ",")
    
    from <- as.integer(trim_from*hz)
    to <- as.integer(trim_to*hz)
    
    to_move <- 
      data[c(from:to),] %>% 
      dplyr::mutate(obs = new_folder)
    
     to_move_options <- 
       options_data %>% 
         dplyr::mutate( obs = new_folder,  
                        processor = NA,
                        include = NA,
                        ## mv2nm = NA,
                        ## nm2pn = NA,
                        analyzer = NA,
                        report = 'not run',
                        review = NA )
     
    setProgress(0.6)
    data.table::fwrite(to_move, file = file.path(new_folder_path, "trap-data.csv"), sep = ',')
    data.table::fwrite(to_move_options, file = file.path(new_folder_path, "options.csv"), sep = ',')
    setProgress(0.8)
    #regroup current observation after desired files moved out
    regroup <- data[-c(from:to),]
    data.table::fwrite(regroup, file = file.path(trap_selected_obs, "trap-data.csv"), sep = ',')
    incProgress(1, detail = "Done")
  })
  showNotification("Files moved to new obs.", type = "message")
}

#' Delete selected data from observation
#' @param trap_selected_obs current selected observation
#' @param trim_from begin index to delete
#' @param trim_to end index to delete
#' @param f the reactiveValues list 'f' with all user selected folder info
trim_obs <- function(trap_selected_obs, trim_from, trim_to, f, hz){
  withProgress(message = "Trimming Data", min= 0, max = 1, value = 0.3, {
    
    path <- list_files(trap_selected_obs, pattern = 'trap-data.csv')
    data <- data.table::fread(path$path, sep = ",")
    
    from <- as.integer(trim_from*hz)
    to <- as.integer(trim_to*hz)
    
    trimmed <- data[-c(from:to),]
    
    setProgress("Writing new 'grouped' file", value = 0.8)
    data.table::fwrite(trimmed, file = file.path(trap_selected_obs, 'trap-data.csv'), sep = ",")
    setProgress("Done", value = 1)
    
  })
}
