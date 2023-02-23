#' Split trap data into observations
#'
#' @param input_data the shiny path to the user uploaded data
#' @param date a file path to the date folder where data should be moved
#' @param threshold a numeric value specifying number of seconds to divide observations by
#' 
#'
#' @noRd


split_obs <- function(input_data, project, conditions, date, threshold, hz = 5000){
  
  withProgress(message = 'Uploading trap data', value = 0, max = 1, min = 0, {
    
    setProgress(0.3, detail = "Reading Files")
    input_data <- dplyr::arrange(input_data, name)
    txts <- purrr::map(input_data$datapath,  ~tibble::as_tibble(data.table::fread(.x, col.names = c("raw_bead", "trap_position"))))
    incProgress(0.5, detail = "Determining Observations")
    extract_numbers <- purrr::map(input_data$name, str_trap)
    datetime <- purrr::map(extract_numbers, lubridate::ymd_hms, tz = "EST")
    
    diff_vector <- vector()
    for(i in seq_along(datetime[-length(datetime)])){
      
      dif <- as.double(difftime(datetime[[i+1]], datetime[[i]], units = "secs"))
      
      if(dif > threshold){
        diff_vector[[i]] <- "end_observation"
      } else {
        diff_vector[[i]] <- "observing"
      }
    }  
    
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
    
    setProgress(0.75, detail = "Arranging Folders")
    
    for(r in 1:nrow(diff_tibble2)){
      if(r < 10){
        dir.create(paste0(date$path, "/obs-0", r))
      } else {
        dir.create(paste0(date$path,"/obs-", r))
      }
      start <-  diff_tibble2$begin_observation.index[[r]] 
      stop <- diff_tibble2$end_observation.index[[r]]

    }
    
    #create obs
    create_obs <- vector("list")
    for(row in 1:nrow(diff_tibble2)){
      go <- diff_tibble2$begin_observation.index[[row]]
      halt <- diff_tibble2$end_observation.index[[row]]
      create_obs[[row]] <- dplyr::bind_rows(txts[go:halt])
    }
    
    o <- data.frame(project = project$name,
                    conditions = conditions$name,
                    date = date$name, 
                    hz = hz,
                    processor = NA,
                    include = NA,
                    mv2nm = NA, 
                    nm2pn = NA,
                    analyzer = NA,
                    report = 'not run',
                    review = NA)
    
    setProgress(0.9, detail = "Saving Data")
    for(c in seq_along(create_obs)){
      if(c < 10){
        t <- data.frame(project = project$name,
                        conditions = conditions$name,
                        date = date$name, 
                        obs = paste0('obs-0', c),
                        raw_bead = create_obs[[c]]$raw_bead,
                        trap_position = create_obs[[c]]$trap_position)
        
        o %<>% mutate(obs =  paste0("obs-0", c)) %>% dplyr::select(project, conditions, date, obs, everything())
        
         data.table::fwrite(t, file = file.path(date$path, paste0("obs-0", c), "trap-data.csv"), sep = ",")
         data.table::fwrite(o, file = file.path(date$path, paste0("obs-0", c), "options.csv"), sep = ",")
      } else {
          t <- data.frame(project = project$name,
                          conditions = conditions$name,
                          date = date$name, 
                          obs = paste0('obs-', c),
                          raw_bead = create_obs[[c]]$raw_bead,
                          trap_position = create_obs[[c]]$trap_position)
        
          o %<>% mutate(obs =  paste0("obs-", c)) %>% dplyr::select(project, conditions, date, obs, everything())
        
        data.table::fwrite(t, file = file.path(date$path, paste0("obs-", c), "trap-data.csv"), sep = ",")
        data.table::fwrite(o, file = file.path(date$path, paste0("obs-", c), "options.csv"), sep = ",")
      }
    }
    setProgress(1, detail = "Done")
  })
  
  showNotification("Obsevations created")
}
