
#' Move selected data to new obs
#' @param trap_selected_date current selected date folder
#' @param trap_selected_obs current selected observation
#' @param trim_from begin index to delete
#' @param trim_to end index to delete
#' @param f the reactiveValues list 'f' with all user selected folder info
#' @param trap_obs obs folder numbers
#'
#' @noRd

move_obs <- function(trap_selected_date, trap_selected_obs, trim_from, trim_to, f, trap_obs){
  withProgress(message = 'Moving Data', value = 0, max = 1, min = 0, {
    #for dev   # trap_selected_date <- '/Users/brentscott/Desktop/2020-01-07'
    #for dev    #trap_selected_obs <- "/Users/brentscott/Desktop/2020-01-07/obs-01"
    #for dev   # number_obs <- nrow(trap_obs)
    #for dev    #trap_obs <- length(list.files(trap_selected_date, pattern = 'obs-'))
    new_obs <- trap_obs + 1
    if(new_obs < 10){
      new_folder <- paste0("obs-", 0, new_obs)
    } else {
      new_folder <- paste0("obs-", new_obs)
    }
    
    new_folder_path <- paste0(trap_selected_date, "/", new_folder)
    incProgress(amount = .25, detail = "Creating new folder")
    dir.create(path = new_folder_path)
    
    setProgress(0.4)
    
    path <- list_files(trap_selected_obs, pattern = 'trap-data.csv')
    data <- vroom::vroom(path$path, delim = ",")
    
    from <- as.integer(trim_from*5000)
    to <- as.integer(trim_to*5000)
    to_move <- data[c(from:to),]
    
    to_move %<>% dplyr::mutate(obs = new_folder)
    
    vroom::vroom_write(to_move, path = file.path(new_folder_path, "trap-data.csv"), delim = ",")
    
    #regroup current observation after desired files moved out
    regroup <- data[-c(from:to),]
  
    # write_csv(regroup, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)
    #vroom::vroom_write(regroup, path = file.path(trap_selected_obs, "trap-data.csv"), delim = ",")
    readr::write_csv(regroup, path = file.path(trap_selected_obs, "trap-data.csv"))
    incProgress(1, detail = "Done")
  })
  showNotification("Files moved to new obs.", type = "message")
}



#' Delete selected data from observation
#'
#' @param trap_selected_obs current selected observation
#' @param trim_from begin index to delete
#' @param trim_to end index to delete
#' @param f the reactiveValues list 'f' with all user selected folder info
#'
#' @noRd

trim_obs <- function(trap_selected_obs, trim_from, trim_to, f){
  
  #trap_grouped_file <- read.csv("/Users/brentscott/Desktop/myoV-WT_2ndConrtol _obs-01/grouped.csv")
  withProgress(message = "Trimming Data", min= 0, max = 1, value = 0.3, {
   
    path <- list_files(trap_selected_obs, pattern = 'trap-data.csv')
    data <- vroom::vroom(path$path, delim = ",")
    
    from <- as.integer(trim_from*5000)
    to <- as.integer(trim_to*5000)
    trimmed <- data[-c(from:to),]
    
    setProgress("Writing new 'grouped' file", value = 0.8)
    #write_csv(trimmed, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)
    
    # t <- create_lasertrapr_tibble( project = f$project$name,
    #                                conditions = f$conditions$name,
    #                                date = f$date$name, 
    #                                obs = f$obs$name,
    #                                raw_bead = trimmed )
  
    vroom::vroom_write(trimmed, path = file.path(trap_selected_obs, 'trap-data.csv'), delim = ",")
    
    
    setProgress("Done", value = 1)
    
  })
}