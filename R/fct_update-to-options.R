#' Update files to new options format
#'
#' @param project_path a character string giving path to a lasertrapr project folder
#' @export
update_to_options <- function(project_path){
  #project_path <- "~/lasertrapr/project_myoV-subset"
  folders_path <- list.dirs(project_path)
  obs_path <- folders_path[grep("obs-", folders_path)]
  
  transfer <- function(obs_path){
    print(obs_path)
    path <- obs_path
    name <- c("trap-data.csv", "options.csv")
    file_names <- file.path(path, name)
    
    trap_data <- data.table::fread(file_names[[1]], nrows = 1)
    
    info_to_transfer <-  suppressWarnings(
      trap_data[, !c("raw_bead", "trap_position", "processed_bead", "hm_overlay")] 
    )
    if(file.exists(file_names[[2]])){
      existing_options <- data.table::fread(file_names[[2]])
      new_options <- dplyr::left_join(info_to_transfer, existing_options)
    } else {
      new_options <- info_to_transfer
    }
    data.table::fwrite(new_options, file = file_names[[2]])
  }
  purrr::walk(obs_path, transfer)
}



#' Add a time column to all trap-data.csv
#'
#' @param project_path project folder path
#' @param hz 
#' @export
add_time_column <- function(project_path, hz = 5000){
  
  path <- list.files(project_path, 
                      "trap-data.csv",
                      recursive = TRUE,
                      full.names = TRUE)
  
  add_time <- function(path, hz){
    trap_data <- data.table::fread(path)
    from_by <- 1/hz
    trap_data[, time_sec := seq(from = from_by, by = from_by, length.out = nrow(trap_data))]
    data.table::fwrite(trap_data, path)
  }
  
  purrr::walk(path, ~add_time(.,hz = hz))
}