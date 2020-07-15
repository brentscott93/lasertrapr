#' Split trap data into observations
#'
#' @param input_data the shiny path to the user uploaded data
#' @param date a file path to the date folder where data should be moved
#' @param threshold a numeric value specifying number of seconds to divide observations by
#' 
#' @import tidyverse 
#' @noRd


split_obs <- function(input_data, project, conditions, date, threshold){
    
  golem::print_dev('starting split_obs')
  # for dev
  # input_data <- tibble::tibble(name = list.files('~/rstats/play/raw-trap-data', full.names = F),
  #                     datapath = list.files('~/rstats/play/raw-trap-data', full.names = T))
  #  date <- '~/Desktop'
  # threshold <- 20

  #convert to csv and move to box sync
  withProgress(message = 'Uploading trap data', value = 0, max = 1, min = 0, {
    
    incProgress(amount = .3, detail = "Reading Files")
    
    input_data <- dplyr::arrange(input_data, name)
    
  #  
  #  s <- c( '~/Desktop/Data_2019_02_27_14_50_34.txt',
  #   '~/Desktop/Data_2019_02_27_14_50_37.csv')
  # 
  # try <-   purrr::map(s, ~tibble::as_tibble(data.table::fread(.x, col.names = c('bead', 'trap'))))
    
 # try2 <- purrr::map(try, tibble::as_tibble)
    #READ
    trap_txts <- purrr::map(input_data$datapath,  ~tibble::as_tibble(data.table::fread(.x, col.names = c("bead", "trap"))))
    
    incProgress(amount = .6, detail = "Moving to 'lasertrapr' folder")
    
    #ss <- stringr::str_sub(s, 1, -4)
    ss <- stringr::str_sub(input_data$name, 1, -4)
    new_csv_filename <- stringr::str_c(ss, 'csv')
    
    new_name <- file.path(date$path, new_csv_filename)
    #for dev new_name <- paste0(date, "/", new_csv_filename)
    purrr::walk2(trap_txts, new_name, readr::write_csv, col_names = TRUE)
    
    incProgress(1, detail = "Done")
  })
  
 # showNotification("Trap Data Uploaded", type = "message")
  
  #split obs
  withProgress(message = 'Making Observations', value = 0, max = 1, min = 0, {
    incProgress(amount = .25, detail = "Reading Data")
    
    all_files <-  list_files(date$path) %>%
      arrange(name)
    
    
    
    file_tibble <- all_files %>%
      dplyr::filter(str_detect(name, "Data"))
    
    txts <- purrr::map(file_tibble$path, read_csv)
    
    incProgress(amount = .4, detail = "Determining Observations")

    extract_numbers <- purrr::map(file_tibble$name, str_trap)
    datetime <- purrr::map(extract_numbers, lubridate::ymd_hms, tz = "EST")
    
    
    
    #for dev
    #threshold <- 20
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
    
    if(identical(incomplete_obs, integer(0)) == F){
      diff_tibble2 <- slice(diff_tibble2, -incomplete_obs)
    }
    
    if(diff_tibble2$observation[[1]] != "end_observation"){
      diff_tibble2 <- slice(diff_tibble2, -1)
    }
  
    
    
    diff_tibble2$observation[[1]] <- "begin_observation"
    for(x in 2:(nrow(diff_tibble2)-1)){
      if(diff_tibble2$observation[[(x-1)]] == "end_observation"){
        diff_tibble2$observation[[x]] <- "begin_observation"
      }
    }
    
    
    
    diff_tibble2 <- diff_tibble2  %<>% 
      filter(observation != "observing") %>% 
      split(.$observation) %>% 
      do.call('cbind', .)
    
    incProgress(.7, detail = "Arranging Folders")
    
    obs_file_names <- vector("list")
    #make new folders
    for(r in 1:nrow(diff_tibble2)){
      
      if(r < 10){
        #dev
        #dir.create(paste0(date, "/obs-0", r))
       dir.create(paste0(date$path, "/obs-0", r))
      } else {
        #dev
       # dir.create(paste0(date,"/obs-", r))
        dir.create(paste0(date$path,"/obs-", r))
      }
      start <-  diff_tibble2$begin_observation.index[[r]] 
      stop <- diff_tibble2$end_observation.index[[r]]
      
      obs_file_names[[r]] <- file_tibble$name[start:stop]
    }
    
   
    
    #move files
    
    for(o in seq_along(obs_file_names)){
      for(file in seq_along(obs_file_names[[o]])){
        if(o < 10){
          file.rename(from =  paste0(date$path, "/", obs_file_names[[o]][[file]]),
                      to = paste0(date$path, "/obs-0", o, "/", obs_file_names[[o]][[file]]))
          
          #dev
          # file.rename(from =  paste0(date, "/", obs_file_names[[o]][[file]]),
          #             to = paste0(date, "/obs-0", o, "/", obs_file_names[[o]][[file]]))
        } else {
          file.rename(from = paste0(date$path, "/", obs_file_names[[o]][[file]]),
                      to = paste0(date$path, "/obs-", o, "/", obs_file_names[[o]][[file]]))

          #dev
          # file.rename(from = paste0(date, "/", obs_file_names[[o]][[file]]),
          #             to = paste0(date, "/obs-", o, "/", obs_file_names[[o]][[file]]))
        }
      }}
    
    
    #create obs
    create_obs <- vector("list")
    for(row in 1:nrow(diff_tibble2)){
      go <- diff_tibble2$begin_observation.index[[row]]
      halt <- diff_tibble2$end_observation.index[[row]]
      create_obs[[row]] <- dplyr::bind_rows(txts[go:halt])
    }
    
    incProgress(.9, detail = "Saving Data")
    # writeLines("Saving Data")
    for(c in seq_along(create_obs)){
      if(c < 10){
        
        t <- create_lasertrapr_tibble( project = project$name,
                                       conditions = conditions$name,
                                       date = date$name, 
                                       obs = paste0('obs-0', c),
                                       grouped = create_obs[[c]],
                                       rds_file_path =  paste0(date$path, "/obs-0", c, "/trap-data.rds"))
        
        saveRDS(t, file = paste0(date$path, "/obs-0", c, "/trap-data.rds"))
         # write_csv(create_obs[[c]],
         #           path = paste0(date, "/obs-0", c, "/grouped.csv"),
         #           col_names = TRUE)
        
      } else {
        
        t <- create_lasertrapr_tibble( project = project$name,
                                       conditions = conditions$name,
                                       date = date$name, 
                                       obs = paste0('obs-', c),
                                       grouped = create_obs[[c]],
                                       rds_file_path =  paste0(date$path, "/obs-", c, "/trap-data.rds"))
        
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
  
  
  
