#' Convenient wrapper to fit Hidden-Markov Model to single molecule myosin data 
#' @description Provides a convenient wrapper to setup and fit an HM-Model to single molecule myosin trapping data. 
#' @param trap_data trap-data.csv 
#' @param run_mean running mean
#' @param run_var running variance
#' @param em_random_start TRUE/FALSE
#' @param is_shiny TRUE/FALSE
#' @return a list of two. 1) a table with running mean, variance, and hm-model identified state. 2) variance signal-to-noise.
fit_hm_model <- function(trap_data, run_mean, run_var, em_random_start, is_shiny, project, conditions, date, obs){


seed <- floor(runif(1, 0, 1e6))

hmm <- depmixS4::depmix(list(run_var~1,
                             run_mean~1),
                        data = data.frame(run_mean = run_mean,
                                          run_var = run_var),
                        nstates = 2,
                        family = list(stats::gaussian(),
                                      stats::gaussian()))


hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                            0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess


sd_run_mean <- sd(run_mean)
mean_run_var <- mean(run_var)
sd_run_var <- sd(run_var)

if(em_random_start == T){
  hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = TRUE))
} else {
  estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, 0, sd_run_mean,
                              mean_run_var/2, sd_run_var, 3, sd_run_mean*2)
  hmm <- depmixS4::setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))
  set.seed(seed)
  hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = FALSE))
}

hmm_posterior <- depmixS4::posterior(hmm_fit)

#make sure HMM starts in state 2 this will reset seed and try to refit 10 times
#should really never have to do this with the em.control set

hmm_repeat <- 0

while(hmm_repeat < 10){
  
  if(hmm_posterior$state[[1]] == 1){
    print("HMM starts in state 1")
    hmm_repeat <- 11
    
  } else if(hmm_posterior$state[[1]] == 2){
    print(paste("Refitting HMM", hmm_repeat))
    
    seed <- floor(runif(1, 0, 1e6))
    
    set.seed(seed)
    
    hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = em_random_start))
    
    hmm_posterior <- depmixS4::posterior(hmm_fit)
    
    hmm_repeat <- hmm_repeat + 1
  }
}




#purposesefully skip data if hm-model starts in state 2. All calculations assume it starts in state 1
if(hmm_posterior$state[[1]] == 2){
  # writeLines(c("Skipping",
  #              trap_data$obs,
  #              "HMM starts in State 2. Try trimming the beginning of the observation."), error_file);
 
  
  obs_trap_data_exit <- trap_data  %>% 
    dplyr::mutate(report = 'hmm-error',
                  analyzer = 'hm/cp')

  vroom::vroom_write(obs_trap_data_exit, path = file.path('~', 'lasertrapr', project, conditions, date, obs, 'trap-data.csv'), delim = ",")
  if(is_shiny) showNotification('Skipping...HM-Model starts in state 2. Try trimming beginnging of obs.', type = 'warning')
  stop("HM-Model Starts in State 2. Try trimming the beginning of the obs.")
} 


sum_fit <- depmixS4::summary(hmm_fit)
base_var <- sum_fit[[1]]
event_var <- sum_fit[[2]]

s2n <- base_var/event_var

#save running mean, var, & state for dygraph
data <- tibble::tibble(run_mean = unname(run_mean),
                       run_var = unname(run_var),
                       state = hmm_posterior$state, 
                       index = 1:length(state),
                       var_signal_ratio = s2n)

return(data)

}







#' Measured events identified by HM-Model
#' @description Takes the results from fit_hm_model() and estiamtes displacements, time on, time off, etc. 
#' @param hm_model_results results of fit_hm_model()
#' @param conversion 
#' @param hz 
#' @param nm2pn 
measure_hm_events <- function(processed_data, hm_model_results, conversion, hz, nm2pn){
  
  #hm_model_results <- hm_model_results
  #convert running mean object to tibble
  # run_mean_tibble <- tibble::enframe(run_mean) %>%
  #   mutate(index = seq(1, length(run_mean), length.out = length(run_mean)))
  # 
  #finds lengths of events in number of running windows
  run_length_encoding <- rle(hm_model_results$state)
  
  #converting to a tibble
  viterbi_rle <- tibble::as_tibble(do.call("cbind", run_length_encoding))
  
  #make a copy of data for time on analysis with only number of windows per event
  time_on_windows <- viterbi_rle %>%
    dplyr::filter(values == 2)
  
  
  #get data for time off analysis
  if(hm_model_results$state[[length(hm_model_results$state)]] == 2){
    
    time_off_windows <- viterbi_rle %>%
      dplyr::filter(values == 1) %>%
      tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
      mutate(off_length_hz = lengths*conversion,
             time_off_ms = (off_length_hz/hz)*1000)
    
  } else {
    
    time_off_windows <- viterbi_rle %>%
      dplyr::filter(values == 1) %>%
      tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
      head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
      mutate(off_length_hz = lengths*conversion,
             time_off_ms = (off_length_hz/hz)*1000)
  }
  
  
  #estimate the events durations & combine with time offs
  on_off_times <- time_on_windows %>%
    dplyr::mutate(index = 1:nrow(time_on_windows),
                  length_hz = lengths*conversion,
                  time_on_ms = (length_hz/hz)*1000,
                  time_off_ms = c(NA, time_off_windows$time_off_ms)) %>%
    dplyr::select(index ,values, everything()) %>%
    dplyr::rename("num_windows" = lengths,
                  "hmm_state" = values)
  
  
  #estimate event displacement
  
  #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
  viterbi_rle_4_step_sizes <- if(tail(viterbi_rle, 1)$values == 1){
    dplyr::slice(viterbi_rle, -length(viterbi_rle$values))
  } else {
    viterbi_rle
  }
  
  #Calculate the cumulative sum of the run length encoder
  #And splits the tibble into two seperate tables to isolate state 1 info from state 2
  split_data <- viterbi_rle_4_step_sizes %>%
    dplyr::mutate(cumsum = cumsum(lengths)) %>%
    dplyr::group_by(values) %>%
    split(viterbi_rle_4_step_sizes$values)
  
  #data is recmombined in a state_1 column and a state_2
  #the values in these columns represent the last window in either state 1 or state 2
  #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the estimate event duration
  hm_event_transitions <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum)
  
  #loop over regrouped data to find the mean of the events displacements
  state_2_avg <- vector("list", length = nrow(hm_event_transitions)) #allocate space for output storage of loop
  peak_nm_index <- vector()
  for(i in 1:nrow(hm_event_transitions)){
    data_subset <- hm_model_results[(hm_event_transitions$state_1_end[i]+1) : (hm_event_transitions$state_2_end[i]),]
    max_step_index <- data_subset$index[which.max(abs(data_subset$run_mean))]
    peak_nm_index[i] <- max_step_index
    state_2_avg[[i]] <- data_subset$run_mean[which(data_subset$index == max_step_index)]
  }
  
  #do opposite to get means of state 1 to subtract from s2 means.
  hm_state_1_transitions <- data.frame(state_1_end = split_data[[1]]$cumsum,
                                       state_1_start = c(0, split_data[[2]]$cumsum[-length(split_data[[2]]$cumsum)]) + 1)
  
  
 
  #loop over s1_regrouped data to find the mean of state 1
  state_1_avg <- vector("list", length = nrow(hm_state_1_transitions)) #allocate space for output storage of loop
    for(i in seq_along(1:nrow(hm_state_1_transitions))){
      state_1_avg[[i]] <- mean(hm_model_results$run_mean[ (hm_state_1_transitions$state_1_start[[i]]) : hm_state_1_transitions$state_1_end[[i]] ])
    }

 

   #esimate step size as difference in states
  calculate_mean_differences <- unlist(state_2_avg) - unlist(state_1_avg) 
  ## DIRECTION CORRECTION ##
  
  positive_events <- sum(calculate_mean_differences > 0)
  negative_events <- sum(calculate_mean_differences < 0)
  
  #if there are more negative step sizes than positive, actin filament assumed backward and all events flipped (multipled by -1)
  #also raw trace, state 1 averages, and step sizes flipped for hmm overlay
 if(negative_events > positive_events){
      flip_mean_differences <- calculate_mean_differences * -1
      flip_raw <-  processed_data * -1
      flip_state_1_avg <- unlist(state_1_avg) * -1
      flip_state_2_avg <-  unlist(state_2_avg) * -1
      
  } else {
      flip_mean_differences <- calculate_mean_differences
      flip_raw <- processed_data
      flip_state_1_avg <- unlist(state_1_avg)
      flip_state_2_avg <-  unlist(state_2_avg)
  }
  
  ends_in_state_1 <- FALSE
  if(viterbi_rle$values[length(viterbi_rle$values)] == 1){
    ends_in_state_1 <- TRUE
    last_s1_start <- split_data[[2]]$cumsum[length(split_data[[2]]$cumsum)] + 1
    end_of_data <- viterbi_rle$values[(length(viterbi_rle$values))]
    last_state_1_avg <- mean( hm_model_results$run_mean[ last_s1_start : end_of_data ] )
      if(negative_events > positive_events){
        last_state_1_avg <- last_state_1_avg * -1
      }
    flip_state_1_avg <- c(flip_state_1_avg, last_state_1_avg)
  }
  
  
 did_it_flip <- negative_events > positive_events
 is_positive <- calculate_mean_differences > 0 
 
 if(did_it_flip == TRUE){
   is_positive <- ifelse(is_positive == TRUE, FALSE, TRUE)
 } 
  #add step sizes and forces to the on_off_times table
  measured_events <- on_off_times %>%
    dplyr::mutate(displacement_nm = flip_mean_differences,
                  force = displacement_nm*nm2pn)
  
  
  return(list(measured_events_hm_estimates = measured_events, 
              flip_raw = flip_raw,
              state_1_avg = flip_state_1_avg,
              state_2_avg = flip_state_2_avg,
              conversion = conversion,
              did_it_flip = did_it_flip,
              is_positive = is_positive,
              viterbi_rle = viterbi_rle, 
              peak_nm_index = peak_nm_index,
              hm_event_transitions = hm_event_transitions,
              ends_in_state_1 = ends_in_state_1))
}



#' Runs changepoint analysis, estimates more accurate event measurements, and saves data for ensemble averaging.
#'
#' @param measured_hm_events the results from measure_hm_events()
#' @param hz sampling frequency in Hz (default is 5000)
#' @param conversion 
#'
#' @return
#' @export
changepoint_and_ensemble <- function(measured_hm_events,  hz,  conversion){
  #for dev
  # flip_raw <- measured_hm_events$flip_raw
  # viterbi_rle <- measured_hm_events$viterbi_rle
  # hm_event_transitions <- measured_hm_events$hm_event_transitions
  # is_positive <- measured_hm_events$is_positive
  flip_raw <- measured_hm_events$flip_raw
  viterbi_rle <- measured_hm_events$viterbi_rle
  hm_event_transitions <- measured_hm_events$hm_event_transitions
  is_positive <- measured_hm_events$is_positive
  state_1_avg <-  measured_hm_events$state_1_avg
  
  trap_data <- tibble(data = flip_raw,
                    index = 1:length(data))
  
  time_prior <- viterbi_rle %>% 
    dplyr::filter(values == 1)
  
  hm_time_ons <- viterbi_rle %>% 
    dplyr::filter(values == 2)
  
  better_time_on_starts <- vector()
  forward_ensemble_average_data <- vector("list")
  ensemble_keep1 <- vector()
  #forward_plots <- list()
  
  better_time_on_stops <- vector()
  backwards_ensemble_average_data <- vector("list")
  ensemble_keep2 <- vector()
  #backwards_plots <- list()
  
  better_displacements <- vector()
  absolute_better_displacements <- vector()
 
  ensemble_length <- hz
  
  for(c in 1:nrow(hm_event_transitions)){
    print(c)
    #get event data chunk
    # forward_chunk <- processed_data_tibble[forward_data$s1_end[[c]] : forward_data$s2_end[[c]],]
    # forward_ensemble_chunk <- run_var_ensemble[forward_data$s1_end[[c]] : forward_data$s2_end[[c]],]
    # 
    # backwards_chunk <- processed_data_tibble[backwards_data$s2_end[[c]] : backwards_data$s1_start[[c]],]
    # backwards_ensemble_chunk <- run_var_ensemble[backwards_data$s2_end[[c]] : backwards_data$s1_start[[c]],]
    
    #event_chunk <- processed_data_tibble[forward_data$s1_end[[c]] :  backwards_data$s1_start[[c]],]
  
    #find how many windows around event we can index out
    #smallest value of time befor/after event and the event duration
    length_of_hm_event <- hm_time_ons$lengths[[c]] #* conversion
    length_of_prior_baseline <- time_prior$lengths[[c]] #* conversion
    length_of_after_baseline <- try(time_prior$lengths[[(c+1)]]) #* conversion)
    
    # if(class(length_of_after_baseline) == "try-error"){
    #   cp_window_length <-  length_of_prior_baseline 
    # } else {
    #   cp_window_length <-  as.numeric(min(length_of_prior_baseline, length_of_after_baseline)) 
    # }
    
    #estimated window index that the event starts and ends at from window time
    estimated_start <- (hm_event_transitions$state_1_end[[c]] + 1 ) 
    estimated_stop <-  hm_event_transitions$state_2_end[[c]] 
    #be a little more aggresive with the estimated start/stop if the cp_window_legnth is small to try and give it as much data as possible
    #this hopefully will be bare minimum needed to capture the transition plus the small amount it will get extra
    # forward_cp_window_start <- (estimated_start - 2) * conversion
    # forward_cp_window_stop <- estimated_start * conversion
    # backwards_cp_window_start <- estimated_stop * conversion
    # backwards_cp_window_stop <- (estimated_stop + 2) * conversion
    
    
    if(length_of_hm_event == 1){
      forward_cp_window_stop <- estimated_start * conversion
      backwards_cp_window_start <- floor((estimated_stop - 0.5) * conversion)
    } else {
      forward_cp_window_stop <- (estimated_start + 1) * conversion
      backwards_cp_window_start <- floor((estimated_stop - 1.5)) * conversion
    }

    if(length_of_prior_baseline == 1) {
      forward_cp_window_start <- (estimated_start - 2) * conversion
    } else if(length_of_prior_baseline == 2) {
      forward_cp_window_start <- (estimated_start - 3) * conversion
    } else {
      forward_cp_window_start <- (estimated_start - 4) * conversion
    }

    if(length_of_after_baseline == 1 | class(length_of_after_baseline) == 'try-error') {
      backwards_cp_window_stop <- ceiling((estimated_stop + 1.5)) * conversion
    } else if(length_of_after_baseline == 2) {
      backwards_cp_window_stop <- ceiling((estimated_stop + 2.5)) * conversion
    } else {
      backwards_cp_window_stop <- ceiling((estimated_stop + 3.5)) * conversion
    }
    
    forward_event_chunk <- trap_data[forward_cp_window_start:forward_cp_window_stop,]
    backwards_event_chunk <- trap_data[backwards_cp_window_start:backwards_cp_window_stop,]
    
    #if data has NA skip - helped reduce errors
    has_na <- table(is.na(forward_event_chunk$data))
    has_na2 <- table(is.na(backwards_event_chunk$data))
    if(length(has_na) > 1){
      better_time_on_starts[[c]] <- NA
      ensemble_keep1[[c]] <- FALSE
      better_displacements[[c]] <- NA
      absolute_better_displacements[[c]] <- NA
    } else {
      forward_cpt_object <- changepoint::cpt.meanvar(forward_event_chunk$data,
                                        penalty = "MBIC",
                                        method = 'AMOC',
                                        minseglen = 5)
    }
    
    if(length(has_na2) > 1){
      better_time_on_stops[[c]] <- NA
      ensemble_keep2[[c]] <- FALSE
    } else {
      backwards_cpt_object <- changepoint::cpt.meanvar(backwards_event_chunk$data,
                                          penalty = "MBIC",
                                          method = 'AMOC',
                                          minseglen = 5)
    }
    
   
    # plot(forward_cpt_object)
    # plot(backwards_cpt_object)
    
    event_on <- try(changepoint::cpts(forward_cpt_object))
    event_off <- try(changepoint::cpts(backwards_cpt_object))
    
    #if no changepoint id'd skip
    if(identical(event_on, numeric(0))  | class(event_on) == 'try-error'){
      better_time_on_starts[[c]] <- NA
      ensemble_keep1[[c]] <- FALSE
    } else {
      #or record change point index
      cp_start <- forward_event_chunk$index[event_on]
      better_time_on_starts[[c]] <- cp_start
      ensemble_keep1[[c]] <- TRUE
     
    }
  
    #do same for backwards
    if(identical(event_off, numeric(0))  | class(event_off) == 'try-error'){
      better_time_on_stops[[c]] <- NA
      ensemble_keep2[[c]] <- FALSE
    } else {
      #or record the index where this occurred in the previous attempt
      cp_off <- backwards_event_chunk$index[event_off]
      better_time_on_stops[[c]] <- cp_off
      ensemble_keep2[[c]] <- TRUE
    }
    
    if(identical(event_on, numeric(0)) | identical(event_off, numeric(0)) | class(event_off) == 'try-error' |  class(event_on) == 'try-error'){
      next
    }
   

    #find length of event
    length_of_event <- nrow(trap_data[cp_start:cp_off,])
    
    if(length_of_event >= ensemble_length){
      #if event longer than 1sec take first 5000ms of event starting from the newly found start of event
      forward_5000 <- tibble(data = trap_data$data[ cp_start : (cp_start + (ensemble_length - 1)) ],
                             ensemble_index = 0:(length(data)-1),
                             event = c)
    } else {
      
      #take average of last 5ms to extend events shorter than 1 second to 1 second
      #OR 
      #take the mean calcualted from the changepoint object
      front_avg <- mean(trap_data$data[(cp_off-25) : cp_off ])
      #front_avg <- processed_data_tibble$data[cp_off-1]
      time_diff <- ensemble_length - length_of_event
      extend_event <- c(trap_data$data[ cp_start : cp_off ], rep(front_avg, time_diff))
      
      forward_5000 <- tibble(data = extend_event,
                             ensemble_index = 0:(length(data)-1),
                             event = c)
    }
    
    
    before_event <- tibble(data = trap_data$data[ (cp_start - 75) : (cp_start - 1) ],
                           ensemble_index = -length(data):-1,
                           event = c)
    
    #get baseline mean prior to event from changepoint object and subtract mean from all values to center at 0
    # baseline_mean_prior <- forward_cpt_obj@param.est$mean[[1]]
    
    forward_5000 %<>% rbind(before_event) %>% 
      dplyr::mutate(is_positive = is_positive[[c]],
                    direction = 'forward') %>% 
      # data = data - baseline_mean_prior) %>%
      dplyr::arrange(ensemble_index)
    
    forward_ensemble_average_data[[c]] <- forward_5000
    
    
    if(length_of_event >= ensemble_length){
      
      backwards_5000 <- tibble(data = trap_data$data[ (cp_off - (ensemble_length-1)) : cp_off ],
                               ensemble_index = length(data):(2*length(data) - 1),
                               event = c)
    } else {
      #get average of first 3ms to extend backwards in time to 1 second
      back_avg <- mean(trap_data$data[cp_start:(cp_start+15)])
      # back_avg <- mean(processed_data_tibble$data[cp_start+2])
      #replace first 1ms (5 datapoints) with 5ms average
      time_diff <- (ensemble_length-length_of_event) + 5
      
      extend_event_back <- c(rep(back_avg, time_diff), trap_data$data[ (cp_start + 5) : (cp_off-1) ])
      
      backwards_5000 <- tibble::tibble(data = extend_event_back,
                                       ensemble_index = length(data):(2*length(data)-1),
                                       event = c)
    }
    
    after_event <- tibble::tibble(data = trap_data$data[ (cp_off+ 1) : (cp_off + 75) ],
                                  ensemble_index = 10000:(10000 + (length(data) -1)),
                                  event = c)
    
    
    backwards_5000 %<>% rbind(after_event) %>%
      mutate(is_positive = is_positive[[c]],
             direction = 'backwards') %>% 
      # data = data - baseline_mean_after) %>%
      arrange(ensemble_index)
    
    backwards_ensemble_average_data[[c]] <- backwards_5000
    
    #get better displacements 
    exact_event_chunk <- trap_data$data[cp_start : cp_off]
    #percent10 <- length_of_event*0.1
    mean_event_step <-  mean( trap_data$data[ (cp_start + 5) : (cp_off - 5) ])
    
    #difference in step sizes nad baseline for table
    better_displacements[[c]] <- mean_event_step - state_1_avg[[c]]
    
    #absolute postion for graph overlay
    absolute_better_displacements[[c]] <- mean_event_step
  }
  
  forward_ensemble_df <- dplyr::bind_rows(forward_ensemble_average_data)
  
  backwards_ensemble_df <- dplyr::bind_rows(backwards_ensemble_average_data)
  
  ensemble_avg_df <- rbind(forward_ensemble_df, backwards_ensemble_df) %>%
    arrange(event)
  

  cp_transitions <- tibble(start = better_time_on_starts,
                           stop = better_time_on_stops,
                           keep1 = ensemble_keep1,
                           keep2 = ensemble_keep2,
                           index = 1:length(start),
                           is_positive = is_positive,
                           displacements = better_displacements) %>%
    mutate(cp_time_on_dp = (stop - start) + 1,
           cp_time_on_ms = (cp_time_on_dp/hz)*1000)
  
  return(list(cp_event_transitions = cp_transitions,
              cp_displacements = better_displacements,
              absolute_displacements = absolute_better_displacements,
              ensemble_data = ensemble_avg_df))
}


#' Event Frequency
#' @noRd
#' @param processed_data A vector of processed trap data
#' @param rle_object A run-length-encoding object
#' @param conversion The conversion between running window time and 5kHz

event_frequency <- function(processed_data, rle_object, conversion, hz = 5000, ends_in_state_1){
  #mini-ensemble dev
   processed_data <- rescaled_raw_data$data 
   rle_object <- id_rescaled_events$rle_object 
   conversion = 1
   hz = hz
   ends_in_state_1 =  id_rescaled_events$ends_in_state_1
  #return a dataframe of event frequencies per each second
  #get the number of seconds in the trace
  seconds_in_trace <- length(processed_data)/hz
  
  #get the start indices for each event
  #events are id'd by HM-Model in running window length/time
  #need to convert those indices back to 5kHz time
  
  #remove the last row if trace ends in state 1
  # because there is no event after the last state 1
  
  if(ends_in_state_1){
  start_event <- rle_object %>%
    dplyr::mutate(cumsum = cumsum(lengths),
                  start_event = (cumsum + 1)*round(conversion)) %>%
    dplyr::filter(values == 1) %>%
    head(-1) %>%
    dplyr::pull(start_event)
  } else {
    start_event <- rle_object %>%
      dplyr::mutate(cumsum = cumsum(lengths),
                    start_event = (cumsum + 1)*round(conversion)) %>%
      dplyr::filter(values == 1) %>%
      #head(-1) %>%
      dplyr::pull(start_event)
  }
  
  end_event <-   rle_object %>%
    dplyr::mutate(cumsum = cumsum(lengths),
                  end_event = (cumsum + 1)*round(conversion)) %>%
    dplyr::filter(values == 2) %>%
    dplyr::pull(end_event)
  
  #make a df where each row has 3 columns:
  #1) event start index
  #2) the index of the start of each second in datapoint
  #3) the index of the end of each second in datapoint
  freq_df <- purrr::map_dfr(start_event, ~tibble::tibble(start_event = .x,
                                                         begin = ((seq_len(seconds_in_trace)*5000)-4999),
                                                         end = seq_len(seconds_in_trace)*5000))
  
  
  end_freq_df <- purrr::map_dfr(end_event, ~tibble::tibble(end_event = .x,
                                                           begin = ((seq_len(seconds_in_trace)*5000)-4999),
                                                           end = seq_len(seconds_in_trace)*5000))
  
  
  #test to see if the event is 'between' or in what second interval
  find_it <- freq_df %>%
    dplyr::mutate(is_in = purrr::pmap_lgl(freq_df, ~dplyr::between(..1, ..2, ..3))) %>%
    dplyr::group_by(begin, end) %>%
    dplyr::summarize(freq_start = sum(is_in)) %>%
    tibble::rownames_to_column('second') %>%
    dplyr::mutate(second = as.numeric(second))
  
  
  find_it_end <- end_freq_df %>%
    dplyr::mutate(is_in = purrr::pmap_lgl(end_freq_df, ~dplyr::between(..1, ..2, ..3))) %>%
    dplyr::group_by(begin, end) %>%
    dplyr::summarize(freq_stop = sum(is_in)) %>%
    tibble::rownames_to_column('second') %>%
    dplyr::mutate(second = as.numeric(second))
  
  
  
  # g1 <- ggplot(find_it, aes(x = second, y = freq_start))+
  #   geom_line(aes(group = 1), color = pink())+
  #   geom_point(color = pink())+
  #   scale_x_continuous('', breaks = seq(0, nrow(find_it), by = 10))+
  #   ylab('Events Start')+
  #   theme_black(base_size = 16)
  # 
  # g2 <- ggplot(find_it_end, aes(x = second, y = freq_stop))+
  #   geom_line(aes(group = 1) , color = purple())+
  #   geom_point(color = purple())+
  #   scale_x_continuous('', breaks = seq(0, nrow(find_it_end), by = 10))+
  #   ylab('Events End')+
  #   theme_black(base_size = 16)
  # 
  # 
  # find_it$freq_stop <- find_it_end$freq_stop
  # find_it$diff <- find_it$freq_start - find_it_end$freq_stop
  # 
  # 
  # g3 <- ggplot(find_it, aes(x = second, y = diff))+
  #   geom_line(aes(group = 1), color = green() )+
  #   geom_point(color = green() )+
  #   scale_x_continuous('Seconds', breaks = seq(0, nrow(find_it), by = 10))+
  #   ylab('Diff')+
  #   theme_black(base_size = 16)
  # 
  # 
  # g <- gridExtra::grid.arrange(g1, g2,g3, ncol = 1)
  # 
  
  
  return(find_it)
  
}
