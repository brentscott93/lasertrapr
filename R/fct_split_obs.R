#' Split trap data into observations
#'
#' @param input_data the shiny path to the user uploaded data
#' @param date a file path to the date folder where data should be moved
#' @param threshold a numeric value specifying number of seconds to divide observations by
#'

split_obs <- function(input_data, project, conditions, date, threshold){
  golem::print_dev('starting split_obs')
  # for dev
  # input_data <- tibble(name = list.files('~/rstats/play/raw-trap-data', full.names = F),
  #                     datapath = list.files('~/rstats/play/raw-trap-data', full.names = T))
  #  date <- '~/lasertrapr/project-test/some_conditions/2020-07-04'
  # threshold <- 20

  #convert to csv and move to box sync
  withProgress(message = 'Uploading trap data', value = 0, max = 1, min = 0, {
    
    incProgress(amount = .3, detail = "Reading Files")
    
    input_data <- arrange(input_data, name)
    
    #READ
    trap_txts <- map(input_data$datapath, read_tsv, col_names = c("bead", "trap"))
    
    incProgress(amount = .6, detail = "Moving to 'lasertrapr' folder")
    
    new_csv_filename <-  map(input_data$name, str_replace, pattern = "txt", replacement = "csv")
    
    new_name <- paste0(date$path, "/", new_csv_filename)
    
    walk2(trap_txts, new_name, write_csv, col_names = TRUE)
    
    incProgress(1, detail = "Done")
  })
  
 # showNotification("Trap Data Uploaded", type = "message")
  
  #split obs
  withProgress(message = 'Making Observations', value = 0, max = 1, min = 0, {
    incProgress(amount = .25, detail = "Reading Data")
    
    all_files <-  list_files(date$path) %>%
      arrange(name)
    
    # if(cal_files == TRUE){
    #   calibration_files <- all_files %>%
    #     dplyr::filter(str_detect(name, "Equi") | str_detect(name, "Step"))
    #   
    #   cal_folder_name <- paste0(date, "/cal")
    #   
    #   dir.create(cal_folder_name)
    #   
    #   cal_new_files_path <- paste0(cal_folder_name,"/", calibration_files$name)
    #   
    #   map2(calibration_files$path, cal_new_files_path, file.rename)
    # }
    
    
    file_tibble <- all_files %>%
      dplyr::filter(str_detect(name, "Data"))
    
    txts <- purrr::map(file_tibble$path, read_csv)
    
    incProgress(amount = .4, detail = "Determining Observations")

    extract_numbers <- purrr::map(file_tibble$name, str_trap)
    datetime <- purrr::map(extract_numbers, lubridate::ymd_hms, tz = "EST")
    
    
    
    
    dif2 <- vector("list") #for troubleshooting
    diff_vector <- vector()
    for(i in seq_along(datetime[-length(datetime)])){
      
      dif <- as.double(difftime(datetime[[i+1]], datetime[[i]], units = "secs"))
      
      if(dif > threshold){
        diff_vector[[i]] <- "end_observation"
      } else {
        diff_vector[[i]] <- "observing"
      }
    }
    
    #dif2 <- vector("list") #for troubleshooting
    # diff_vector <- vector()
    # for(i in seq_along(extract_numbers[-length(extract_numbers)])){
    #   dif <- extract_numbers[[i+1]] - extract_numbers[[i]]
    #   dif2[[i]] <- extract_numbers[[i+1]] - extract_numbers[[i]] #for troubleshooting
    
    #   if(dif > cutoff){
    #    diff_vector[[i]] <- "end_observation"
    #  } else {
    #    diff_vector[[i]] <- "observing"
    #  }
    # }
    
    diff_vector[[length(datetime)]] <- "end_observation"
    
    
    diff_tibble2 <- tibble(index = 1:length(diff_vector),
                           observation = diff_vector)
    
    
    
    incomplete_obs <- filter(diff_tibble2, observation == "end_observation" & lag(observation) == "end_observation") %>%
      pull(index)
    
    if(identical(incomplete_obs, integer(0)) == TRUE){
      diff_tibble2 <- diff_tibble2
      
    } else {
      diff_tibble2 <- slice(diff_tibble2, -incomplete_obs)
    }
    
    if(diff_tibble2$observation[[1]] == "end_observation"){
      diff_tibble2 <- slice(diff_tibble2, -1)
    } else {
      diff_tibble2 <- diff_tibble2
    }
  
    
    
    diff_tibble2$observation[[1]] <- "begin_observation"
    for(x in 2:(nrow(diff_tibble2)-1)){
      if(diff_tibble2$observation[[x-1]] == "end_observation"){
        diff_tibble2$observation[[x]] <- "begin_observation"
      }
    }
    
    
    
    diff_tibble2 <- filter(diff_tibble2, observation != "observing") %>%
      group_split(observation) %>%
      bind_cols()
    
    incProgress(.7, detail = "Arranging Folders")
    
    obs_file_names <- vector("list")
    #make new folders
    for(r in 1:nrow(diff_tibble2)){
      
      if(r < 10){
        dir.create(paste0(date$path, "/obs-0", r))
      } else {
        dir.create(paste0(date$path,"/obs-", r))
      }
      obs_file_names[[r]] <- file_tibble$name[diff_tibble2$index[[r]]:diff_tibble2$index1[[r]]]
    }
    
    
    #move files
    
    for(o in seq_along(obs_file_names)){
      for(file in seq_along(obs_file_names[[o]])){
        if(o < 10){
          file.rename(from =  paste0(date$path, "/", obs_file_names[[o]][[file]]),
                      to = paste0(date$path, "/obs-0", o, "/", obs_file_names[[o]][[file]]))
        } else {
          file.rename(from = paste0(date$path, "/", obs_file_names[[o]][[file]]),
                      to = paste0(date$path, "/obs-", o, "/", obs_file_names[[o]][[file]]))
        }
      }}
    
    
    #create obs
    create_obs <- vector("list")
    for(row in 1:nrow(diff_tibble2)){
      create_obs[[row]] <- dplyr::bind_rows(txts[diff_tibble2$index[[row]]:diff_tibble2$index1[[row]]])
    }
    
    incProgress(.9, detail = "Saving Data")
    # writeLines("Saving Data")
    for(c in seq_along(create_obs)){
      if(c < 10){
        
        t <- tibble(project = project$name ,
                    conditions = conditions$name,
                    date = date$name, 
                    grouped = list(create_obs[[c]]))
        
        saveRDS(t, file = paste0(date$path, "/obs-0", c, "/trap-data.rds"))
         # write_csv(create_obs[[c]],
         #           path = paste0(date, "/obs-0", c, "/grouped.csv"),
         #           col_names = TRUE)
        
      } else {
        
        t <- tibble(project = project$name ,
                    conditions = conditions$name,
                    date = date$path, 
                    grouped = list(create_obs[[c]]),
                    include = 'No',
                    processed_how = 'Needs Processing',
                    )
        
        saveRDS(t, file = paste0(date$path, "/obs-", c, "/trap-data.rds"))
        # write_csv(create_obs[[c]],
        #           path = paste0(date, "/obs-", c, "/grouped.csv"),
        #           col_names = TRUE)
        
      }
    }
    
   
    
    incProgress(1, detail = "Done")
  })
  
  showNotification("Obsevations created")
}