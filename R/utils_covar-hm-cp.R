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
                                mean_covar_smooth/4, sd_covar_smooth/4)
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

#' Runs changepoint analysis, estimates more accurate event measurements, and saves data for ensemble averaging.
#'
#' @param pb_data data frame containing the processed beads
#' @param hz sampling frequency in Hz
#' @param nm2pn calibration
#' @param nm2pn2 calibration
#' @noRd
covar_changepoint <- function(pb_data,
                              hm_event_transitions,
                              hz,
                              nm2pn,
                              nm2pn2,
                              value1,
                              w_width,
                              ws,
                              median_w_width,
                              front_cp_method,
                              back_cp_method,
                              look_for_cp_in_between){
## browser()
## pb_data <- data.table::data.table(pb1, pb2)
cp_results <- vector("list")
  did_it_flip_vec <- vector()
for(b in seq_len(2)){
  current_pb <- pb_data[[b]]
  trap_data <- data.table::data.table(index = seq_along(current_pb), data = current_pb)

  event_starts <- vector()
  cp_found_start <- vector()
  event_stops <- vector()
  cp_found_stop <- vector()
  keep_event <- vector()
  displacements_relative <- vector()
  displacements_absolute <- vector()
  displacements_marker <- vector()
  attachment_durations_dp <- vector()
  attachment_durations_s <- vector()
  attachment_durations_ms <- vector()
  forces_absolute <- vector()
  forces_relative <- vector()
  baseline_position_before <- vector()
  v_mean_base_prior_1ms <- vector()
  v_absolute_substep1 <- vector()
  v_relative_substep1 <- vector()
  v_mean_base_after_1ms <- vector()
  v_absolute_substep2 <- vector()
  v_relative_substep2 <- vector()

##   ms1_dp <- hz/1000

  for(i in 1:nrow(hm_event_transitions)){
    print(i)
## ##     #estimated window index that the event starts and ends at from window time
##     estimated_start <- (hm_event_transitions$state_1_end[[i]] + 1 ) * ws
##     estimated_stop <-  hm_event_transitions$state_2_end[[i]] * ws

##       forward_cp_window_start <- estimated_start - (1.5*ws) #- 10
##       forward_cp_window_stop <- estimated_start + (1.5*ws) #

##       backwards_cp_window_start <- estimated_stop - (1.5*ws) #
##        backwards_cp_window_stop <- estimated_stop + (1.5*ws)

    ## length_of_hm_event <- hm_time_ons$lengths[[i]] #* conversion
    length_of_prior_baseline <- value1$lengths[[i]] #* conversion
    length_of_after_baseline <- try(value1$lengths[[(i+1)]]) #* conversion)

    #estimated window index that the event starts and ends at from window time
    estimated_start <- (hm_event_transitions$state_1_end[[i]] + 1 )
    estimated_stop <-  hm_event_transitions$state_2_end[[i]]
    #if(length_of_hm_event == 1){
      forward_cp_window_stop <- ceiling((estimated_start * ws) + ws)
    #} else {
    #  forward_cp_window_stop <- ceiling(((estimated_start+1) * conversion))
   # }
      backwards_cp_window_start <- floor((estimated_stop * ws) - ws)
   # }
    # else {
    #   forward_cp_window_stop <- (estimated_start + 1) * conversion
    #   backwards_cp_window_start <- floor((estimated_stop - 1.5)) * conversion
    # }

    if(length_of_prior_baseline == 1) {
      forward_cp_window_start <- floor((estimated_start - 1.5) * ws)
   } else {
      forward_cp_window_start <- floor(((estimated_start - 2) * ws) - ws)
   }

    if(length_of_after_baseline == 1 || class(length_of_after_baseline) == 'try-error'){
       backwards_cp_window_stop <- ceiling((estimated_stop + 1.5)) * ws
    } else {
       backwards_cp_window_stop <- ceiling((estimated_stop + 2.5)) * ws
    }

    forward_event_chunk <- trap_data[forward_cp_window_start:forward_cp_window_stop,]

    ## ggplot(forward_event_chunk, aes(x = index))+
    ##   geom_line(aes(y = processed_bead_1), color = "red")+
    ##   geom_line(aes(y = processed_bead_2))


    if(backwards_cp_window_stop >= nrow(trap_data)) backwards_cp_window_stop <- nrow(trap_data)

    backwards_event_chunk <- trap_data[backwards_cp_window_start:backwards_cp_window_stop,]

   ##  ## ggplot(backwards_event_chunk, aes(x = index))+
   ##    ## geom_line(aes(y = processed_bead_1), color = "red")+
   ##    ## geom_line(aes(y = processed_bead_2))

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

   ##  #if no changepoint id'd skip
    if(identical(event_on, numeric(0))  || class(event_on) == 'try-error'){
      event_starts[[i]] <- NA
      cp_found_start[[i]] <- FALSE
    } else {
      #or record change point index
      cp_start <- forward_event_chunk$index[(event_on+1)]
      event_starts[[i]] <- cp_start
      cp_found_start[[i]] <- TRUE

    }

    #do same for backwards
    if(identical(event_off, numeric(0))  || class(event_off) == 'try-error'){
      event_stops[[i]] <- NA
      cp_found_stop[[i]] <- FALSE
    } else {
      #or record the index where this occurred in the previous attempt
      cp_off <- backwards_event_chunk$index[event_off]
      event_stops[[i]] <- cp_off
      cp_found_stop[[i]] <- TRUE
    }

    if(identical(event_on, numeric(0)) ||
       identical(event_off, numeric(0)) ||
       class(event_off) == 'try-error' ||
       class(event_on) == 'try-error'){
            displacements_relative[[i]] <- NA
            displacements_absolute[[i]] <- NA
            displacements_marker[[i]] <- NA
      attachment_durations_s[[i]] <- NA
      attachment_durations_dp[[i]] <- NA
      attachment_durations_ms[[i]] <- NA
    v_mean_base_prior_1ms[[i]] <- NA
    v_absolute_substep1[[i]] <- NA
    v_relative_substep1[[i]] <- NA
    v_mean_base_after_1ms[[i]] <- NA
    v_absolute_substep2[[i]] <- NA
    v_relative_substep2[[i]] <- NA
      forces_absolute[[i]] <- NA
      forces_relative[[i]] <- NA
            ## trap_stiffness[[i]] <- NA
            ## myo_stiffness[[i]] <- NA
            keep_event[[i]] <- FALSE
            ## front_var_ratio[[i]] <- NA
            ## back_var_ratio[[i]] <- NA
            next
    }

                                        #find length of event
    length_of_event <- nrow(trap_data[cp_start:(cp_off),])
    golem::print_dev(paste0("length of data is: ",  nrow(trap_data[cp_start:(cp_off),]) ) )
                                        #get a logical if event duration is less than 1 or if either of the changepoints were not found
                                        #this indicates something unusual about the event and we probably do not want it in the analysis
    if(length_of_event <= 0 ||  cp_found_start[[i]] == FALSE || cp_found_stop[[i]] == FALSE || cp_start >= cp_off){
      keep <- FALSE
      displacements_relative[[i]] <- NA
      displacements_absolute[[i]] <- NA
      displacements_marker[[i]] <- NA
      attachment_durations_s[[i]] <- NA
      attachment_durations_dp[[i]] <- NA
      attachment_durations_ms[[i]] <- NA
      v_mean_base_prior_1ms[[i]] <- NA
      v_absolute_substep1[[i]] <- NA
      v_relative_substep1[[i]] <- NA
      v_mean_base_after_1ms[[i]] <- NA
      v_absolute_substep2[[i]] <- NA
      v_relative_substep2[[i]] <- NA
      forces_absolute[[i]] <- NA
      forces_relative[[i]] <- NA
      ## trap_stiffness[[i]] <- NA
      ## myo_stiffness[[i]] <- NA
      keep_event[[i]] <- FALSE
      next
    } else {
      keep <- TRUE
      keep_event[[i]] <- TRUE
    }

## periods_df <- data.frame(start = event_starts, stop = event_stops)

##     dygraphs::dygraph(data.frame(seq_along(pb1), pb1, pb2)) |> #raw_data_dygraph
##       ## dygraphs::dySeries('raw', color = 'black') |>
##       ## dygraphs::dySeries('model', color = "#1B9E77",  strokeWidth = 2) |>
##       dygraphs::dyRangeSelector(fillColor ='white', strokeColor = 'black') |>
##       add_shades(periods_df) #raw_periods
                                        #add_shades(excluded_events, color = "#BDBDBD") %>%
      ## add_labels_hmm(labels, labelLoc = 'bottom') |> #results$events
      ## dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) |>
      ## dygraphs::dyAxis('y', label = 'nm', drawGrid = FALSE) |>
      ## dygraphs::dyUnzoom()
    if(i == 1) {
      base_prior_stop <- cp_start - 1
      base_prior <- trap_data$data[1 : base_prior_stop]
    } else {
      if(!is.na(event_stops[i-1])){
        base_prior_start <- event_stops[[(i-1)]] + 1
        base_prior_stop <- cp_start - 1
        base_prior <- trap_data$data[base_prior_start:base_prior_stop]
      } else {
        base_prior_start <- cp_start - hz/1000
        base_prior_stop <- cp_start - 1
        base_prior <- trap_data$data[base_prior_start:base_prior_stop]
      }
    }

    if(b == 1){
      trap_stiff <- nm2pn
    } else {
      trap_stiff <- nm2pn2
    }
## browser()
    ms1 <- (hz/1000)*2
    mean_event_step <-  mean(trap_data$data[cp_start:(cp_off)])
    mean_base_prior <- mean(base_prior)

    mean_base_prior_1ms <- mean(trap_data$data[(cp_start-1):((cp_start-ms1)-1)])
    absolute_substep1 <- mean(trap_data$data[cp_start:(cp_start+ms1)])
    relative_substep1 <- mean(trap_data$data[cp_start:(cp_start+ms1)]) - mean_base_prior_1ms

    mean_base_after_1ms <- mean(trap_data$data[(cp_off+1):((cp_off+ms1)+1)])
    absolute_substep2 <- mean(trap_data$data[cp_off:(cp_off-ms1)])
    relative_substep2 <- mean(trap_data$data[cp_off:(cp_off-ms1)]) - absolute_substep1

    v_mean_base_prior_1ms[[i]] <- mean_base_prior_1ms
    v_absolute_substep1[[i]] <- absolute_substep1
    v_relative_substep1[[i]] <- relative_substep1

    v_mean_base_after_1ms[[i]] <- mean_base_after_1ms
    v_absolute_substep2[[i]] <- absolute_substep2
    v_relative_substep2[[i]] <- relative_substep2

    baseline_position_before[[i]] <- mean_base_prior
    displacements_absolute[[i]] <- mean_event_step
    displacements_relative[[i]] <- mean_event_step - mean_base_prior
    displacements_marker[[i]]  <- mean(c(cp_start, cp_off))
    attachment_durations_dp[[i]] <- length(cp_start:(cp_off))
    attachment_durations_s[[i]] <- length(cp_start:(cp_off))/hz
    attachment_durations_ms[[i]] <- length(cp_start:(cp_off))/hz*1000
    forces_absolute[[i]] <- trap_stiff*(mean_event_step)
    forces_relative[[i]] <- trap_stiff*(mean_event_step-mean_base_prior)
  } #loop close


 greater_than_0 <- sum(na.omit(displacements_relative >= 0))
 less_than_0 <- sum(na.omit(displacements_relative <= 0))

##  did_it_flip <- FALSE
##  if(greater_than_0 < less_than_0){
##    displacements_absolute <- displacements_absolute*-1
##    displacements_relative <- displacements_relative*-1
##    forces_absolute <- forces_absolute*-1
##    forces_relative <- forces_relative*-1
##    baseline_position_before <- baseline_position_before*-1
##    did_it_flip <- TRUE
##  }
## print(paste0("b = ", b, " /did_it_flip = ", did_it_flip))
## print(paste0("greater_than_0 = ", greater_than_0, "less_than_0 = ", less_than_0))
## did_it_flip_vec[[b]] <- did_it_flip

 ## print(did_it_flip_vec)

 df_names <- c(
   paste0("substep_1_bead_", b, "_nm") ,
   paste0("absolute_substep_1_bead_", b, "_nm") ,
   paste0("baseline_prior_1ms_bead_", b, "_nm") ,
   paste0("substep_2_bead_", b, "_nm") ,
   paste0("absolute_substep_2_bead_", b, "_nm") ,
   paste0("baseline_after_1ms_bead_", b, "_nm") ,
   paste0("displacement_bead_", b, "_nm") ,
   paste0("absolute_displacement_bead_", b, "_nm"),
   paste0("displacement_marker_", b) ,
   paste0("attachment_duration_bead_",b, "_dp") ,
   paste0("attachment_duration_bead_",b, "_s"),
   paste0("attachment_duration_bead_",b, "_ms"),
   paste0("force_baed_", b, "_pn"),
   paste0("absolute_force_bead_", b, "_pn"),
   paste0("keep_", b),
   paste0("cp_event_start_dp_", b),
   paste0("cp_event_stop_dp_", b),
   paste0("cp_found_start_", b),
   paste0("cp_found_stop_", b)
 )

 measured_bead <- data.table::data.table(
                                v_relative_substep1,
                                v_absolute_substep1,
                                v_mean_base_prior_1ms,
                                v_relative_substep2,
                                v_absolute_substep2,
                                v_mean_base_after_1ms,
                                displacements_relative,
                                displacements_absolute,
                                displacements_marker,
                                attachment_durations_dp,
                                attachment_durations_s,
                                attachment_durations_ms,
                                forces_relative,
                                forces_absolute,
                                keep_event,
                                event_starts,
                                event_stops,
                                cp_found_start,
                                cp_found_stop)

 names(measured_bead) <- df_names

 cp_results[[b]] <- measured_bead


if(!look_for_cp_in_between) next

 ib_results <- vector("list")
 in_between_cp_found <- vector()
  for(j in seq_along(displacements_relative)){

    if(!keep_event[[j]]) next

    in_between_chunk <- trap_data[event_starts[[j]]:event_stops[[j]]]

    ## chunk_roll <- na.omit(RcppRoll::roll_median(in_between_chunk$data, n = 10))

    in_between_cp <- try(
      changepoint::cpt.mean(as.vector(scale(na.omit(in_between_chunk$data))),
                               penalty = "MBIC",
                             ## pen.value = 1,
                               method = 'AMOC',
                             minseglen = 200
                            )
      )

       try(plot(in_between_cp))

    in_between_cp_location <- try(changepoint::cpts(in_between_cp))

   ##  #if no changepoint id'd skip
    if(identical(in_between_cp_location, numeric(0))  || class(in_between_cp_location) == 'try-error'){
      in_between_cp_found[[j]] <- FALSE
      next
    }
      #or record change point index
      in_between_cp_found[[j]] <- TRUE
    # Change Point In Between
      cpib <- in_between_chunk$index[in_between_cp_location]

    # In Between step 1
      ib1 <- trap_data$data[event_starts[[j]]:cpib]
    # In Between step 2
      ib2 <- trap_data$data[cpib:event_stops[[j]]]

 if(did_it_flip){
   ib1 <- ib1*-1
   ib2 <- ib2*-1
 }

    ib_between_table <-
      data.table::data.table(
                    event = j,
                    bead = b,
                    ib_start = event_starts[[j]],
                    ib_mid = cpib,
                    ib_stop = event_stops[[j]],
                    ib1_absolute = mean(ib1),
                    ib1_relative = mean(ib1)-baseline_position_before[[j]],
                    ib2_absolute = mean(ib2),
                    ib2_relative = mean(ib2)-mean(ib1),
                    ib1_time_dp = length(ib1),
                    ib1_time_s = length(ib1)/hz,
                    ib1_time_ms = length(ib1)/hz*1000,
                    ib2_time_dp = length(ib2),
                    ib2_time_s = length(ib2)/hz,
                    ib2_time_ms = length(ib2)/hz*1000
                    )

    ib_results[[j]] <- ib_between_table

    } #in between close

 ib_results_dt <- data.table::rbindlist(ib_results)

     ib_filename <- file.path(path.expand("~"),
                              "lasertrapr",
                              project,
                              conditions,
                              date,
                              obs,
                              paste0("in-between-bead-", b, ".csv"))
    data.table::fwrite(ib_results_dt, ib_filename, sep = ",")

  } # beadclose




  return(
    list(data = cbind(cp_results[[1]], cp_results[[2]]),
         did_it_flip_vec = did_it_flip_vec)
    )




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
 } #fun close
