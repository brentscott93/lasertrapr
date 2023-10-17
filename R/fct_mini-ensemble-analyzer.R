
#' Mini Ensemble Analyzer
#'
#' @param opt an options.csv file
#' @param w_width_ms window width in ms
#' @param displacement_threshold minimum displacement threshold
#' @param time_threshold_ms minimum time on threshold
#' @param f f reactive from shiny app
mini_ensemble_analyzer <- function(opt, w_width_ms = 10, displacement_threshold = 8, time_threshold_ms = 10, f){
  #for dev
  # opt <- data.table::fread("/home/brent/lasertrapr/project_tm-mutants/eWT_pH-7.4_pCa-6.5/2021-09-20/obs-01/options.csv")
  # w_width_ms = 10
  # displacement_threshold = 8
  # time_threshold_ms = 10
  
  
  project <- unique(opt$project)
  conditions <- unique(opt$conditions)
  date <- unique(opt$date)
  obs <- unique(opt$obs)
  include <- unique(opt$include)
  if(is.na(include)) include <- FALSE
  mv2nm <-  opt$mv2nm
  nm2pn <- opt$nm2pn
  hz <- opt$hz
  
  
  error_file <- file(file.path(f$date$path, "error-log.txt"), open = "a")
  tryCatch({
    # files <- list_files("~/lasertrapr/project_mini/mini/2020-08-04", pattern = 'trap-data.csv', recursive = T)
    # trap_data <- purrr::map(files$path, vroom::vroom)
    # trap_data <- trap_data[[1]]
    if(!include){
      obs_opt_data_exit <-
        opt  %>%
        dplyr::mutate(report = 'user-excluded',
                      analyzer = 'none',
                      review = F)
      
      data.table::fwrite(obs_opt_data_exit, path = file.path(path.expand('~'),
                                                             'lasertrapr', 
                                                             project,
                                                             conditions, 
                                                             date, 
                                                             obs, 
                                                             'options.csv'), 
                         sep = ",")
      
      stop("User Excluded")
    }
    trap_data_path <- file.path(path.expand('~'),
                               'lasertrapr', 
                               project,
                               conditions, 
                               date, 
                               obs, 
                               'trap-data.csv')
    trap_data <- data.table::fread(trap_data_path)
    not_ready <- rlang::is_empty(trap_data$processed_bead)
    if(not_ready){
      if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
      stop('Data not processed')
    }
    
     report_data <- 'error'
     w_width <- ms_to_dp(w_width_ms, hz = hz)
     time_threshold <- ms_to_dp(time_threshold_ms, hz = hz)
     processed_data <- trap_data$processed_bead
   
    #calculate running mean
    setProgress(0.25, detail = 'Calculating Running Mean')
    run_mean <- na.omit(RcppRoll::roll_meanl(processed_data, n = w_width))
    run_mean0 <- ifelse(run_mean < 0, 0, run_mean)
    
    setProgress(0.4, detail = 'Identifying Events')
    id_events <- id_mini_events(run_mean = run_mean,
                               displacement_threshold = displacement_threshold, 
                               time_threshold = time_threshold)
    
    events <- id_events$events
    
    setProgress(0.6, detail = 'Rescaling Data')
    scale_by_event_index <- data.frame(state_1_start = c(0, events$state_2_end[-length(events$state_2_end)] + 1),
                                       state_2_end = events$state_2_end)
    
    prior_noise_plus_event <- vector("list")
    for(i in 1:nrow(scale_by_event_index)){
      prior_noise_plus_event[[i]] <- processed_data[scale_by_event_index$state_1_start[i]:scale_by_event_index$state_2_end[i]]
    }
    
    state_1_index <-  data.frame(state_1_start = scale_by_event_index$state_1_start,
                                 state_1_end = events$state_1_end)
    
    state_1_means <- vector()
    for(i in 1:nrow(state_1_index)){
      state_1_means[i] <- mean(processed_data[state_1_index$state_1_start[i]:state_1_index$state_1_end[i]])
    }
    
    rescaled_vectors <- vector("list")
    for(i in 1:length(prior_noise_plus_event)){
      rescaled_vectors[[i]] <- prior_noise_plus_event[[i]] - state_1_means[i]
    }
    
    ##### FIND BETTER START OF EVENT########
    end_of_last_event <- max(length(events$state_2_end))
    last_s1_start <- events$state_2_end[end_of_last_event]+ 1
    end_raw <- length(processed_data)
    
    if(id_events$ends_in_state_1){
      rescaled_raw_data <-  c(unlist(rescaled_vectors), processed_data[last_s1_start : end_raw])
    } else {
      rescaled_raw_data <-  unlist(rescaled_vectors)
    }
    
    if(length(rescaled_raw_data) != length(processed_data)){
      p1 <- length(rescaled_raw_data) + 1
      p2 <- length(processed_data)
      rescaled_raw_data <- c(rescaled_raw_data, processed_data[p1:p2])
    }
    
    rescaled_raw_data <- data.frame(data = rescaled_raw_data,
                                    index = 1:length(rescaled_raw_data))
    
    setProgress(0.65, detail = 'Calculate Rescaled Running Mean')
    run_mean_rescaled <- na.omit(RcppRoll::roll_meanl(rescaled_raw_data$data, n =  w_width))
    run_mean_rescaled0 <- ifelse(run_mean_rescaled < 0 , 0, run_mean_rescaled)
    
    setProgress(0.75, detail = 'Identifying rescaled events')
    id_rescaled_events <- id_mini_events(run_mean_rescaled,
                                         displacement_threshold = displacement_threshold, 
                                         time_threshold = time_threshold)
  
    ##### FIND OFF TIMES #######
    rescaled_events <- id_rescaled_events$events
    
    minus1 <- rescaled_events$state_1_end[-1]
    minus2 <- rescaled_events$state_2_end[-length(rescaled_events$state_2_end)]
    
    off_time_index <- 
      dplyr::bind_cols(state_1_start = minus2 + 1, state_1_end = minus1) %>%
      dplyr::mutate(off_time_dp = (state_1_end - state_1_start) +1,
             off_time_sec = off_time_dp/hz,
             off_time_ms = off_time_sec*1000)
    
    ###### FORCES #####
    
    peak_displacement <- vector()
    peak_nm_index <- vector()
    for(i in 1:nrow(rescaled_events)){
      temp_vector <- rescaled_raw_data[(rescaled_events$state_1_end[i] + 1) : (rescaled_events$state_2_end[i]),]
      run_mean_event <- tibble::tibble(data = na.omit(RcppRoll::roll_meanl(temp_vector$data, n = 10)),
                                       index = min(temp_vector$index):(min(temp_vector$index) + (length(data)-1)))
      peak_window <- max(run_mean_event$data)
      peak_displacement[i] <- peak_window
      get_index_in_chunk <- which(run_mean_event$data==max(run_mean_event$data))[[1]]
      get_index_in_data <- run_mean_event$index[[get_index_in_chunk]]
      peak_nm_index[i] <- get_index_in_data 
    }
    
    ##### COMBINE ALL EVENT DATA ####
    rescaled_events$force <-  peak_displacement*nm2pn
   
    final_events <-  
      rescaled_events %>%
      dplyr::mutate(off_time_prior_dp = c(NA, off_time_index$off_time_dp),
             off_time_prior_sec = off_time_prior_dp/hz,
             time_off_ms = off_time_prior_sec * 1000,
             time_on_dp = state_2_end - state_1_end,
             time_on_sec = time_on_dp/hz,
             time_on_ms = time_on_sec * 1000,
             displacement_nm = peak_displacement,
             conditions = conditions,
             project = project, 
             conditions = conditions,
             date = date, 
             obs = obs,
             hz = hz, 
             event_start = state_1_end + 1,
             peak_nm_index = peak_nm_index,
             index = 1:nrow(rescaled_events)) %>% 
      dplyr::rename("event_stop" = state_2_end) %>%
      dplyr::select(project,
                    conditions,
                    date, 
                    obs, 
                    time_off_ms, 
                    time_on_ms,
                    displacement_nm, 
                    force, 
                    index, 
                    event_start, 
                    event_stop, 
                    peak_nm_index)
    
    setProgress(0.85, detail = 'Calculating event frequency')
    event_freq <- mini_event_frequency(rescaled_raw_data$data,  
                                      id_rescaled_events$events, 
                                      conversion = 1,
                                      hz = hz, 
                                      ends_in_state_1 =  id_rescaled_events$ends_in_state_1)
   
   times <- length(rescaled_raw_data$data) - length(run_mean_rescaled0)
   report_data = 'success'
   
   setProgress(0.95, detail = 'Saving Data')
   trap_data <-
     dplyr::mutate(trap_data,
            rescaled_mini_data = rescaled_raw_data$data,
            run_mean_overlay = c(run_mean_rescaled0, rep(0, times)))
   
   options_df <-
     opt %>% 
      dplyr::mutate(analyzer = "mini", 
                    report = report_data,
                    w_width_ms = w_width_ms, 
                    displacement_threshold = displacement_threshold, 
                    time_threshold_ms = time_threshold_ms)
   
    data_to_save <- list(final_events, event_freq, trap_data, options_df)
    filenames <- c('mini-measured-events.csv',
                   'mini-event-frequency.csv',
                   'trap-data.csv',
                   'options.csv')
    path <- file.path(path.expand('~'), 'lasertrapr', project, conditions, date, obs)
    purrr::walk2(data_to_save, filenames, ~data.table::fwrite(.x, file = file.path(path, .y), sep = ","))
    
     }, error=function(e){
       if(!include){
         showNotification(paste0("Skipping ", obs, ' user excluded'), type = 'message', duration = 2)
       } else {
    showNotification(ui = paste0("Analysis error in ",
                            date,
                            " ",
                            conditions,
                            " ",
                            obs,
                            ". Error Message: ",
                            as.character(e)), type = 'warning', duration = NULL)
    writeLines(
      paste0("Analysis error in ",
             date,
             " ",
             conditions,
             " ",
             obs,
             ". Error Message: ",
             as.character(e)), 
      error_file)
       }
  })
  
  close(error_file)
  setProgress(1, detail = "Done!")
  
  return(invisible())
}

    
    
    
    
    
#' Identify mini-ensemble events
#'
#' @param data trap data
#' @param run_mean calculated running mean
#' @param displacement_threshold threshold to determine events (x)
#' @param time_threshold threshold to determine events (y)
#' #'
#' @return df of events
#' @export
id_mini_events <- function(run_mean, displacement_threshold, time_threshold){
      #Determine if run_mean is in an event or baseline noise by using >10 as event
      on_off <- ifelse(run_mean > displacement_threshold, 2, 1)
      
      rle_object<- tibble::as_tibble(do.call("cbind", rle(on_off)))
      
      if(length(unique(rle_object$values)) == 1){
        stop('No events in trace. There is either one events or the thresholds are too stringent.')
      }
      #if starts in state2/event get rid of it
      if(rle_object$values[[1]] == 2){
        add_to_s1 <- rle_object$lengths[[1]]
        rle_object <- rle_object |> dplyr::slice(2:nrow(rle_object))
        rle_object$lengths[[1]] <- rle_object$lengths[[1]] + add_to_s1
      }
      
      ends_in_s1 <- rle_object$values[length(rle_object$values)] == 1
      
      #find initial event start/stop
      #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
      mini_rle_object <- 
        if(tail(rle_object, 1)$values == 1){
        dplyr::slice(rle_object, -length(rle_object$values))
         } else {
        rle_object
      }
      
      split_data <- 
        mini_rle_object %>%
        dplyr::mutate(cumsum = cumsum(lengths)) %>%
        dplyr::group_by(values) %>%
        split(mini_rle_object$values)
      
      #data is recombined in a state_1 column and a state_2
      #the values in these columns represent the last data point in either state 1 or state 2
      #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
      regroup_data <- dplyr::bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum) %>%
        dplyr::mutate(event_duration_dp = state_2_end - state_1_end)
      
      #filter out state 2s that are less than threshold
      events <- 
        regroup_data %>% 
        dplyr::filter(event_duration_dp > time_threshold)
      
      return(list(events = events, 
             rle_object = rle_object,
             ends_in_state_1 = ends_in_s1))
}

#' Mini - Ensemble Event Frequency
#' @noRd
#' @param rescaled_raw_data A vector of rescaled mini ensemble trap data
#' @param rle_object A run-length-encoding object
#' @param conversion The conversion between running window time and 5kHz

mini_event_frequency <- function(rescaled_raw_data, events, conversion, hz, ends_in_state_1){
  #mini-ensemble dev
  # rescaled_raw_data <- rescaled_raw_data$data 
  # events <- id_rescaled_events$rle_object 
  # conversion = 1
  # hz = hz
  # ends_in_state_1 =  id_rescaled_events$ends_in_state_1
  #return a dataframe of event frequencies per each second
  #get the number of seconds in the trace
  seconds_in_trace <- length(rescaled_raw_data)/hz
  
  start_event <- events$state_1_end + 1
  #make a df where each row has 3 columns:
  #1) event start index
  #2) the index of the start of each second in datapoint
  #3) the index of the end of each second in datapoint
  freq_df <- purrr::map_dfr(start_event, ~tibble::tibble(start_event = .x,
                                                         begin = ((seq_len(seconds_in_trace)*hz)-(hz-1)),
                                                         end = seq_len(seconds_in_trace)*hz))
  
  
  #test to see if the event is 'between' or in what second interval
  find_it <- freq_df %>%
    dplyr::mutate(is_in = purrr::pmap_lgl(freq_df, ~dplyr::between(..1, ..2, ..3))) %>%
    dplyr::group_by(begin, end) %>%
    dplyr::summarize(freq_start = sum(is_in)) %>%
    tibble::rownames_to_column('second') %>%
    dplyr::mutate(second = as.numeric(second))

  # find_it_end <- end_freq_df %>%
  #   dplyr::mutate(is_in = purrr::pmap_lgl(end_freq_df, ~dplyr::between(..1, ..2, ..3))) %>%
  #   dplyr::group_by(begin, end) %>%
  #   dplyr::summarize(freq_stop = sum(is_in)) %>%
  #   tibble::rownames_to_column('second') %>%
  #   dplyr::mutate(second = as.numeric(second))
  # 
  return(find_it)
  
}


    
    

    
    
