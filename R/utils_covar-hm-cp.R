#' Convenient wrapper to fit Hidden-Markov Model to COVARIANCE
#' @description Provides a convenient wrapper to setup and fit an HM-Model to single molecule myosin trapping data.
#' @param covar_smooth smoothed covariance
#' @param em_random_start TRUE/FALSE
#' @param is_shiny TRUE/FALSE
#' @return a list of two. 1) a table with running mean, variance, and hm-model identified state. 2) variance signal-to-noise.
fit_hm_model_to_covar_smooth <- function(covar_smooth,
                                         em_random_start,
                                         is_shiny,
                                         project,
                                         conditions,
                                         date,
                                         obs){

  seed <- floor(runif(1, 0, 1e6))

    hmm <- depmixS4::depmix(covar_smooth~1,
                            data = data.frame(covar_smooth = covar_smooth),
                            nstates = 2,
                            family = stats::gaussian())


  hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                              0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                              0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess


  sd_covar_smooth <- sd(covar_smooth)
  mean_covar_smooth <- mean(covar_smooth)
 ## em_random_start <- T
  if(em_random_start == T){
    hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = TRUE))
  } else {
    estimate_hmm_gaussians <- c(mean_covar_smooth, sd_covar_smooth,
                                mean_covar_smooth/2, sd_covar_smooth/5)
    hmm <- depmixS4::setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))
    set.seed(seed)
    hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = FALSE))
  }

  hmm_posterior <- depmixS4::posterior(hmm_fit, type = "viterbi")

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
      hmm_posterior <- depmixS4::posterior(hmm_fit, type = "viterbi")
      hmm_repeat <- hmm_repeat + 1
    }
  }




  #purposesefully skip data if hm-model starts in state 2. All calculations assume it starts in state 1
  if(hmm_posterior$state[[1]] == 2){
    # writeLines(c("Skipping",
    #              trap_data$obs,
    #              "HMM starts in State 2. Try trimming the beginning of the observation."), error_file);
    obs_trap_data_exit <- trap_data  |>
      dplyr::mutate(report = 'hmm-error',
                    analyzer = 'hm/cp')
    data.table::fwrite(obs_trap_data_exit, file = file.path(path.expand('~'), 'lasertrapr', project, conditions, date, obs, 'trap-data.csv'), sep = ",")
    if(is_shiny) showNotification('Skipping...HM-Model starts in state 2. Try trimming beginnging of obs.', type = 'warning')
    stop("HM-Model Starts in State 2. Try trimming the beginning of the obs.")
  }


  sum_fit <- depmixS4::summary(hmm_fit)
  base_covar <- sum_fit[[1]]
  event_covar <- sum_fit[[2]]

  s2n <- base_covar/event_covar

  #save running mean, var, & state for dygraph
  data <- data.table::data.table(covar_smooth = unname(covar_smooth),
                                 state = hmm_posterior$state,
                                 index = seq_along(covar_smooth),
                                 var_signal_ratio = s2n)

  return(data)
}

## #' Runs changepoint analysis, estimates more accurate event measurements, and saves data for ensemble averaging.
## #'
## #' @param measured_hm_events the results from measure_hm_events()
## #' @param hz sampling frequency in Hz
## #' @param conversion
## #' @param value1 viterbi_rle data.table with only state 1
## #' @noRd
## changepoint_analysis_covar <- function(
##                                        trap_data,
##                                        hm_event_transitions,
##                                        hz,
##                                        value1,
##                                        conditions,
##                                        ws){

##   event_starts <- vector()
##   #forward_ensemble_average_data <- vector("list")
##   cp_found_start <- vector()
##   #forward_plots <- list()

##   event_stops <- vector()
##   #backwards_ensemble_average_data <- vector("list")
##   cp_found_stop <- vector()
##   #backwards_plots <- list()
##   keep_event <- vector()

##   displacements_relative <- vector()
##   displacements_absolute <- vector()
##   displacement_marker <- vector()

##   ms1_dp <- hz/1000

##   for(i in 1:nrow(hm_event_transitions)){
##     print(i)

##     #estimated window index that the event starts and ends at from window time
    estimated_start <- (hm_event_transitions$state_1_end[[i]] + 1 )
    estimated_stop <-  hm_event_transitions$state_2_end[[i]]

      forward_cp_window_stop <- estimated_start + 200

      backwards_cp_window_start <- estimated_stop - 10


    length_of_prior_baseline <- value1$lengths[[i]] #* conversion
    length_of_after_baseline <- try(value1$lengths[[(i+1)]]) #* conversion)

    ## if(length_of_prior_baseline <= 100) {
      ## forward_cp_window_start <- estimated_start - 50
   ## } else {
      forward_cp_window_start <- estimated_start  - 10
   ## }

    if(length_of_after_baseline <= 300 || class(length_of_after_baseline) == 'try-error'){
       backwards_cp_window_stop <- estimated_stop + 90
    } else {
       backwards_cp_window_stop <- estimated_stop + 200
    }


     trap_data <- data.table(processed_bead_1 = pb1, processed_bead_2 = pb2, index = seq_along(pb1), data = pb1)

    forward_event_chunk <- trap_data[forward_cp_window_start:forward_cp_window_stop,]

    ## ggplot(forward_event_chunk, aes(x = index))+
      ## geom_line(aes(y = processed_bead_1), color = "red")+
      ## geom_line(aes(y = processed_bead_2))


    if(backwards_cp_window_stop >= nrow(trap_data)) backwards_cp_window_stop <- nrow(trap_data)

    backwards_event_chunk <- trap_data[backwards_cp_window_start:backwards_cp_window_stop,]

    ## ggplot(backwards_event_chunk, aes(x = index))+
      ## geom_line(aes(y = processed_bead_1), color = "red")+
      ## geom_line(aes(y = processed_bead_2))

    #if data has NA skip - helped reduce errors
    has_na <- table(is.na(forward_event_chunk$data))
    has_na2 <- table(is.na(backwards_event_chunk$data))
    if(length(has_na) > 1){
      # better_time_on_starts[[i]] <- NA
      # cp_found_start[[i]] <- FALSE
      # better_displacements[[i]] <- NA
      # absolute_better_displacements[[i]] <- NA
      keep_event[[i]] <- FALSE
      next
    } else {
      if(front_cp_method == "Variance"){
        forward_cpt_object <- changepoint::cpt.mean(na.omit(forward_event_chunk$run_var),
                                                    penalty = "MBIC",
                                                    method = 'AMOC')
      } else {
        forward_cpt_object <- changepoint::cpt.meanvar(na.omit(forward_event_chunk$data),
                                                    penalty = "MBIC",
                                                    method = 'AMOC')
      }
    }

    if(length(has_na2) > 1){
      # better_time_on_stops[[i]] <- NA
      # cp_found_stop[[i]] <- FALSE
      keep_event[[i]] <- FALSE
    } else {
      if(back_cp_method == "Variance"){
        backwards_cpt_object <- changepoint::cpt.mean(na.omit(backwards_event_chunk$run_var),
                                                       penalty = "MBIC",
                                                       method = 'AMOC')
      } else {
        backwards_cpt_object <- changepoint::cpt.meanvar(na.omit(backwards_event_chunk$data),
                                                         penalty = "MBIC",
                                                         method = 'AMOC')
      }
    }


    ## plot(forward_cpt_object)
    ## plot(backwards_cpt_object)

    event_on <- try(changepoint::cpts(forward_cpt_object))
    event_off <- try(changepoint::cpts(backwards_cpt_object))

    #if no changepoint id'd skip
    if(identical(event_on, numeric(0))  || class(event_on) == 'try-error'){
      better_time_on_starts[[i]] <- NA
      cp_found_start[[i]] <- FALSE
    } else {
      #or record change point index
      cp_start <- forward_event_chunk$index[event_on]
      better_time_on_starts[[i]] <- cp_start
      cp_found_start[[i]] <- TRUE

    }

    #do same for backwards
    if(identical(event_off, numeric(0))  || class(event_off) == 'try-error'){
      better_time_on_stops[[i]] <- NA
      cp_found_stop[[i]] <- FALSE
    } else {
      #or record the index where this occurred in the previous attempt
      cp_off <- backwards_event_chunk$index[event_off]
      better_time_on_stops[[i]] <- cp_off-1
      cp_found_stop[[i]] <- TRUE
    }

##   ##   if(identical(event_on, numeric(0)) ||
##   ##      identical(event_off, numeric(0)) ||
##   ##      class(event_off) == 'try-error' ||
##   ##      class(event_on) == 'try-error'){
##   ##           better_displacements[[i]] <- NA
##   ##           trap_stiffness[[i]] <- NA
##   ##           myo_stiffness[[i]] <- NA
##   ##           keep_event[[i]] <- FALSE
##   ##           front_var_ratio[[i]] <- NA
##   ##           back_var_ratio[[i]] <- NA
##   ##           next
##   ##   }


##   ##   #find length of event
##   ##   length_of_event <- nrow(trap_data[cp_start:(cp_off-1),])
##   ##   golem::print_dev(paste0("length of data is: ",  nrow(trap_data[cp_start:(cp_off-1),]) ) )
##   ##   #get a logical if event duration is less than 1 or if either of the changepoints were not found
##   ##   #this indicates something unusual about the event and we probably do not want it in the analysis
##   ##   if(length_of_event <= 0 ||  cp_found_start[[i]] == FALSE || cp_found_stop[[i]] == FALSE || cp_start >= cp_off){
##   ##     keep <- FALSE
##   ##     better_displacements[[i]] <- NA
##   ##     trap_stiffness[[i]] <- NA
##   ##     myo_stiffness[[i]] <- NA
##   ##     keep_event[[i]] <- FALSE
##   ##     next
##   ##   } else {
##   ##     keep <- TRUE
##   ##     keep_event[[i]] <- TRUE
##   ##   }

##   ##   ms_5 <- 5*hz/1000
##   ##   before_data <- trap_data$data[ (cp_start - ms_5) : (cp_start - 1) ]
##   ##   before_var <- var(before_data)
##   ##   first_25 <-  trap_data$data[ (cp_start + ms1_dp) : (cp_start + (ms_5 + ms1_dp)) ]
##   ##   front_var <- var(first_25)
##   ##   front_var_ratio[[i]] <- before_var/front_var

##   ##   event_back_25 <- trap_data$data[ (cp_off - (ms_5 + (ms1_dp + 1 ))) : (cp_off - (ms1_dp + 1)) ]
##   ##   event_back_var <- var(event_back_25)
##   ##   after_25 <- trap_data$data[ cp_off : (cp_off + (ms_5-1)) ]
##   ##   after_25_var <- var(after_25)

##   ##   back_var_ratio[[i]] <- after_25_var/event_back_var

##   ##   if(displacement_type == "avg"){
##   ##     mean_event_step <-  mean(trap_data$data[(cp_start + ms_5):((cp_off-1) - ms_5)])
##   ##     displacement_mark <- (measured_hm_events$peak_nm_index[[i]]*conversion)/hz
##   ##   } else if(displacement_type == "peak"){
##   ##      cp_event_subset <- trap_data[cp_start:(cp_off-1),]
##   ##      cp_event_subset$roll_mean <- RcppRoll::roll_meanr(cp_event_subset$data, hz/1000*5)
##   ##     if(is_positive[[i]]){
##   ##      mean_event_step <- max(na.omit(cp_event_subset$roll_mean))
##   ##      max_row <- cp_event_subset[which(cp_event_subset$roll_mean == mean_event_step),]
##   ##      displacement_mark <- max_row$index[[1]]/hz
##   ##     } else {
##   ##       mean_event_step <- min(na.omit(cp_event_subset$roll_mean))
##   ##       min_row <- cp_event_subset[which(cp_event_subset$roll_mean == mean_event_step),]
##   ##       displacement_mark <- min_row$index[[1]]/hz
##   ##     }
##   ##   }

##   ##   #difference in step sizes nad baseline for tablems
##   ##   better_displacements[[i]] <- mean_event_step - state_1_avg[[i]]

##   ##   #absolute postion for graph overlay
##   ##   absolute_better_displacements[[i]] <- mean_event_step

##   ##   displacement_marker[[i]] <- displacement_mark

##   ##   #estimate myosin and trap stiffness
##   ##   quarter <- length_of_event/4
##   ##   myo_event_chunk <- trap_data$data[(cp_start + quarter) : ( cp_off - quarter )]
##   ##   if(i == 1) {
##   ##     base_prior_stop <- better_time_on_starts[[i]] - 1
##   ##     base_prior <- trap_data$data[1 : base_prior_stop]
##   ##     trap_stiffness[[i]] <- equipartition(base_prior)
##   ##     myo_stiffness[[i]] <- equipartition(myo_event_chunk)
##   ##   } else {
##   ##     base_prior_start <- better_time_on_stops[[(i-1)]] + 1
##   ##     if(is.na(base_prior_start)){
##   ##       trap_stiffness[[i]] <- NA
##   ##       myo_stiffness[[i]] <- NA
##   ##     } else {
##   ##     base_prior_stop <- better_time_on_starts[[i]] - 1
##   ##     base_prior <- trap_data$data[base_prior_start : base_prior_stop]
##   ##     trap_stiffness[[i]] <- equipartition(base_prior)
##   ##     myo_stiffness[[i]] <- equipartition(myo_event_chunk)
##   ##     }
##   ##   }

##   ## } #loop close

##   ## cp_transitions <- tibble::tibble(start = better_time_on_starts,
##   ##                                  stop = better_time_on_stops,
##   ##                                  cp_found_start = cp_found_start,
##   ##                                  cp_found_stop = cp_found_stop,
##   ##                                  index = 1:length(start),
##   ##                                  is_positive = is_positive,
##   ##                                  keep = keep_event,
##   ##                                  cp_displacements = better_displacements,
##   ##                                  trap_stiffness = trap_stiffness,
##   ##                                  myo_stiffness = myo_stiffness,
##   ##                                  front_signal_ratio = front_var_ratio,
##   ##                                  back_signal_ratio = back_var_ratio) %>%
##   ##   dplyr::mutate(cp_time_on_dp = (stop - start) + 1,
##   ##                 cp_time_on_ms = (cp_time_on_dp/hz)*1000)

##   ## return(list(cp_event_transitions = cp_transitions,
##   ##             cp_displacements = better_displacements,
##   ##             absolute_displacements = absolute_better_displacements,
##   ##             displacement_mark = displacement_marker))
## }
