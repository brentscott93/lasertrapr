
#' Create new observation with selected data
#'
#' @param f the reactiveValues list 'f' with all user selected folder info
#' @param trap_selected_date path to current date folder
#' @param trap_obs all trap observations
#' @param trap_selected_obs selected observation
#' @param trap_files all raw Data...csv files in selected obsrevation
#' @param dygraph_clean_date_window_1 beginning index to move
#' @param dygraph_clean_date_window_2 end index to move
#' @import tidyverse magrittr

move_obs <- function(f, trap_selected_date, trap_obs, trap_selected_obs,  trap_files, dygraph_clean_date_window_1, dygraph_clean_date_window_2){
  #make destination folder
  withProgress(message = 'Moving Files', value = 0, max = 1, min = 0, {
    
   # number_obs <- nrow(trap_obs)
    
    new_obs <- trap_obs + 1
    
    if(new_obs < 10){
      new_folder <- paste0("obs-", 0, new_obs)
    } else {
      new_folder <- paste0("obs-", new_obs)
    }
    
    new_folder_path <- paste0(trap_selected_date, "/", new_folder)
    incProgress(amount = .25, detail = "Creating new folder")
    dir.create(path = new_folder_path)
    
    #identify folders on drive and move
    start_of_file_indices <- seq(0,
                                 by = 5,
                                 length.out = nrow(trap_files))
    
    move_files_from <- round_any(dygraph_clean_date_window_1,
                                 5,
                                 f = floor)
    
    from_index <- which(start_of_file_indices == move_files_from)
    
    end_of_file_indices <- seq(5,
                               by = 5,
                               length.out = nrow(trap_files))
    
    move_files_to <- round_any(dygraph_clean_date_window_2,
                               5,
                               f = ceiling)
    
    to_index <- which(end_of_file_indices == move_files_to)
    
    files_to_move <- dplyr::slice(trap_files, from_index:to_index)
    
    files_to_move_paths <- files_to_move %>%
      dplyr::pull(path)
    
    files_to_move_names <-  files_to_move %>%
      dplyr::pull(name)
    
    new_files_names <- paste0(new_folder_path, "/", files_to_move_names)
    
    incProgress(amount = .75, detail = "Moving files")
    
    purrr::map2(files_to_move_paths, new_files_names, file.rename)
    
    new_paths <- list_files(new_folder_path) %>%
      dplyr::filter(str_detect(name, "Data")) %>%
      dplyr::pull(path)
    
    new_obs_files <- bind_rows(map(new_paths, read_csv))
    
    t <- tibble(project = f$project$name ,
                conditions = f$conditions$name,
                date = f$date$name, 
                grouped = list(new_obs_files))
    
   
    
    saveRDS(t, file = file.path(new_folder_path, "trap-data.rds"))
    #regroup current observation after desired files moved out
    
    existing_files <- list_dir(trap_selected_obs) %>%
      dplyr::filter(str_detect(name, "Data"))
    
   
    
    regroup <- bind_rows(map(existing_files$path, read_csv))
    
    regroup_t <- tibble(project = f$project$name ,
                        conditions = f$conditions$name,
                        date = f$date$name, 
                        grouped = list(regroup))
    
   # write_csv(regroup, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)
    saveRDS(regroup_t, file = file.path(trap_selected_obs, "trap-data.rds"))
    incProgress(1, detail = "Done")
  })
  showNotification("Files moved to new obs.", type = "message")
  
}




#' Delete selected data from observation
#'
#' @param trap_selected_obs current selected observation
#' @param trap_grouped_file grouped file of currently selected observation
#' @param trim_from begin index to delete
#' @param trim_to end index to delete
#' @param f the reactiveValues list 'f' with all user selected folder info
#' @import tidyverse magrittr

trim_obs <- function(trap_selected_obs, trap_grouped_file, trim_from, trim_to, f){
  
  #trap_grouped_file <- read.csv("/Users/brentscott/Desktop/myoV-WT_2ndConrtol _obs-01/grouped.csv")
  withProgress(message = "Trimming Data", min= 0, max = 1, value = 0.3, {
   
    from <- as.integer(trim_from*5000)
    to <- as.integer(trim_to*5000)
    trimmed <- trap_grouped_file[-c(from:to),]
    
    setProgress("Writing new 'grouped' file", value = 0.8)
    #write_csv(trimmed, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)
    t <- tibble(project = f$project$name ,
                conditions = f$conditions$name,
                date = f$date$name, 
                grouped = list(trimmed))
    
    
    
    saveRDS(t, file = file.path(trap_selected_obs, "trap-data.rds"))
    
    
    setProgress("Done", value = 1)
    
  })
}