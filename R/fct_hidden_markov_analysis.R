
#' Hidden markov analysis
#' @noRd
#' @param trap_data_rds A dataframe of all 'trap-date.rds' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.


hidden_markov_analysis <- function(trap_data_rds, f, em_random_start, is_shiny = F){
 # withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
    # param trap_data_rds A dataframe containing the row-binded trap-data.rds files
    # aram f The f reactive value object containing all folder file paths and names
    # param em_random_start Logical. TRUE/FALSE
    # import changepoint
    #  
    # return Performs hidden markov
    #for dev
    #loaded in trap-data.rds thorugh file manager in RStudio
    #same code in the observer in app before this function call
    # trap_data_rds <- get_status_table(f$date, f$date_input)

   # trap_data_rds <- trap
    #rds_file_path <- list_files('~/lasertrapr/project_new/new/2020-07-13/obs-01', pattern = 'trap-data.rds', recursive = T)
    #trap_data_rds %<>% mutate(rds_file_path = rds_file_path$path )
    #em_random_start <- FALSE
    #trap_data_rds %<>% dplyr::filter(include == T)
    
    hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                                0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                                0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess

    error_file <- file(file.path(f$date$path, "error-log.txt"), open = "a")
    #loop will start 
    for(folder in seq_along(trap_data_rds$obs)){
      tryCatch({
        not_ready <- is_empty(trap_data_rds$processed[[folder]])
        if(not_ready == T){
          if(is_shiny == T) showNotification(paste0(trap_data_rds$obs[[folder]], ' not processed. Skipping...'), type = 'warning')
          next
          }
                  
        report_data <- "failed_to_initialize"
    
        if(is_shiny == T) setProgress(0.05, paste("Analyzing", trap_data_rds$conditions[[folder]], trap_data_rds$obs[[folder]]))
        
        
        
        mv2nm <- trap_data_rds$mv2nm[[folder]]
        nm2pn <- trap_data_rds$nm2pn[[folder]]
        
        processed_data <- trap_data_rds$processed[[folder]]$bead
        
        #### RUNNING MEAN & VAR ####
        w_width <- 150
        
        if(is_shiny == T) setProgress(0.1, detail = "Calculating Running Mean")
        
        run_mean <- na.omit(RcppRoll::roll_meanl(processed_data, n = w_width, by = w_width/2))
        
        if(is_shiny == T) setProgress(0.15, detail = "Calculating Running Variance")
        run_var <- na.omit(RcppRoll::roll_varl(processed_data, n = w_width, by = w_width/2))
      
        running_table <- tibble(run_mean = run_mean,
                                run_var = run_var)
        
        #### HMM ####
        if(is_shiny == T) setProgress(0.18, detail = "HM-Model")
        report_data  <- "failed_HMM"
        
        seed <- floor(runif(1, 0, 1e6))
        
        hmm <- depmixS4::depmix(list(run_var~1,
                                     run_mean~1),
                                data = running_table,
                                nstates = 2,
                                family = list(stats::gaussian(),
                                              stats::gaussian()))
        
        
        
        
        
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
            writeLines("HMM starts in state 1")
            hmm_repeat <- 11
            
          } else if(hmm_posterior$state[[1]] == 2){
            writeLines(paste("Refitting HMM", hmm_repeat))
            
            seed <- floor(runif(1, 0, 1e6))
            
            set.seed(seed)
            
            hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = em_random_start))
            
            hmm_posterior <- depmixS4::posterior(hmm_fit)
            
            hmm_repeat <- hmm_repeat + 1
          }
        }
        
        report_data <- "error: HMM_starts_in_state_2"
        
        if(hmm_posterior$state[[1]] == 2){
          writeLines(c("Skipping",
                       trap_data_rds$obs[[folder]],
                       "HMM starts in State 2."), error_file);
          if(is_shiny)showNotification('Skipping...HM-Model starts in state 2', type = 'warning')
          obs_trap_data_exit <- trap_data_rds[folder, ] %>% 
            dplyr::mutate(results = NA,
                          report = report_data,
                          status = 'analysis-error',
                          analyzer = NA)
          
          
          saveRDS(obs_trap_data_exit, file = trap_data_rds$rds_file_path[[folder]])
          next
        } 
        

        sum_fit <- depmixS4::summary(hmm_fit)
        base_var <- sum_fit[[1]]
        event_var <- sum_fit[[2]]
        
        var_signal_to_noise <- base_var/event_var

        if(is_shiny == T) setProgress(0.25, detail = "HM-Model Complete")

        
        
        ## COUNT EVENTS ##
        incProgress(detail = "Measuring Events")
        
        #save running mean, var, & state for dygraph
        hmm_identified_events <- tibble(run_mean = unname(run_mean),
                                        run_var = unname(run_var),
                                        state = hmm_posterior$state)
        
        
        report_data <- "error_measureing_events"
        ## MEASURE EVENTS ##
        ## Calculate conversion between window length and data points
        #setup
       # conversion <- length(processed_data)/length(run_mean)
        
        conversion <- w_width/2
        
        #convert running mean object to tibble
        run_mean_tibble <- tibble::enframe(run_mean) %>%
          mutate(index = seq(1, length(run_mean), length.out = length(run_mean)))
        
        #finds lengths of events in number of running windows
        run_length_encoding <- rle(hmm_posterior$state)
        
        #converting to a tibble
        rle_object <- as_tibble(do.call("cbind", run_length_encoding))
        
        #make a copy of data for time on analysis
        rle_object_4_duration <- rle_object %>%
          dplyr::filter(values == 2)
        
        if(is_shiny == T) setProgress(0.4)
        
        if(hmm_posterior$state[[length(hmm_posterior$state)]] == 2){
          #make a copy of data for time off analysis
          time_offs <- rle_object %>%
            dplyr::filter(values == 1) %>%
            tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
            # head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
            mutate(off_length_5kHz = lengths*conversion,
                   time_off_ms = (off_length_5kHz/5000)*1000)
        } else {
          #make a copy of data for time off analysis
          time_offs <- rle_object %>%
            dplyr::filter(values == 1) %>%
            tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
            head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
            mutate(off_length_5kHz = lengths*conversion,
                   time_off_ms = (off_length_5kHz/5000)*1000)
        }
        
        
        #estimate the events durations
        on_off_times <- rle_object_4_duration %>%
          dplyr::mutate(n_event = 1:nrow(rle_object_4_duration),
                        length_5kHz = lengths*conversion,
                        time_on_ms = (length_5kHz/5000)*1000,
                        time_off_ms = c(NA, time_offs$time_off_ms)) %>%
          dplyr::select(n_event,values, everything()) %>%
          dplyr::rename("num_windows" = lengths,
                        "hmm_state" = values)
        
        
        #calculate event displacement
        
        #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
        rle_object_4_step_sizes <- if(tail(rle_object, 1)$values == 1){
          dplyr::slice(rle_object, -length(rle_object$values))
        } else {
          rle_object
        }
        #Calculate the cumulative sum of the run length encoder
        #And splits the tibble into two seperate tables to isolate state 1 info from state 2
        
        split_data <- rle_object_4_step_sizes %>%
          dplyr::mutate(cumsum = cumsum(lengths)) %>%
          dplyr::group_by(values) %>%
          split(rle_object_4_step_sizes$values)
        
        #data is recmombined in a state_1 column and a state_2
        #the values in these columns represent the last data point (in window lengths) in either state 1 or state 2
        #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
        regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum)
        
        
        if(is_shiny == T) setProgress(0.5)
        #loop over regrouped data to find the mean of the events displacements
        step_sizes <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
        peak_nm_index <- vector()
        for(i in 1:nrow(regroup_data)){
          
          win_values_t <- run_mean_tibble[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i]),]
          max_step_index <- win_values_t$index[which.max(abs(win_values_t$value))]
          peak_nm_index[i] <- max_step_index
          step_sizes[[i]] <-   win_values_t$value[which(win_values_t$index == max_step_index)]
          
          
        }
        
        #do opposite to get means of state 1 to subtract from s2 means.
        # need to subtract first s1 value and last s2 value of the cumsum to align properly
        #don't need the end point of the first s1 because we don't know when the last event ended because we only have knowledge
        #of events we observe when data collection starts
        
        minus1 <- split_data[[1]]$cumsum[-1]
        minus2 <- split_data[[2]]$cumsum[-length(split_data[[2]]$cumsum)]
        
        
        s1_regroup_data <- bind_cols(state_2_end = minus2, state_1_end = minus1)
        
        #loop over s1_regrouped data to find the mean of state 1
        state_1_avg <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
        state_1_avg[[1]] <- mean(run_mean_tibble$value[1:regroup_data$state_1_end[1]]) #get everage of first state 1
        if(nrow(s1_regroup_data) > 1){
          for(i in seq_along(1:nrow(s1_regroup_data))){
            state_1_avg[[i+1]] <- mean(run_mean_tibble$value[(s1_regroup_data$state_2_end[i]+1) : (s1_regroup_data$state_1_end[i])])
          }
        }
        
        calculate_mean_differences <- tibble(avg_s1 = unlist(state_1_avg),
                                             avg_s2 = unlist(step_sizes),
                                             diff = avg_s2 - avg_s1)
        ## DIRECTION CORRECTION ##
        
        positive_events <- sum(calculate_mean_differences$diff > 0)
        negative_events <- sum(calculate_mean_differences$diff < 0)
        
        #if there are more negative step sizes than positive, actin filament assumed backward and all events flipped (multipled by -1)
        #also raw trace, state 1 averages, and step sizes flipped for hmm overlay
        direction_correction <- if(negative_events > positive_events){
          calculate_mean_differences$diff * -1
        } else {
          calculate_mean_differences$diff
        }
        
        
        flip_raw <- if(negative_events > positive_events){
          processed_data * -1
        } else {
          processed_data
        }
        
        flip_state_1_avg <- if(negative_events > positive_events){
          unlist(state_1_avg) * -1
        } else {
          unlist(state_1_avg)
        }
        
        flip_step_sizes <- if(negative_events > positive_events){
          unlist(step_sizes) * -1
        } else {
          unlist(step_sizes)
        }
        
        
        #add step sizes and forces to the on_off_times table
        measured_events <- on_off_times %>%
          dplyr::mutate(displacement_nm = direction_correction,
                        force = displacement_nm*nm2pn)
        
        
        if(is_shiny == T) setProgress(0.6)
        #find better time on
        
       
        
        
        # event_length <- regroup_data %>%
        #    mutate(event_length = (state_2_end - state_1_end) * round(conversion))
        
        
        processed_data_tibble <- tibble(data = as.vector(flip_raw),
                                        index = seq(1, length(flip_raw), length.out = length(flip_raw)))
        
        #run_var_ensemble <-  running(processed_data_tibble$data, fun = var, width = 50, align = "left")
        #RcppRoll waaay faster
        run_var_ensemble <- tibble(run_var_5 = na.omit(RcppRoll::roll_varl(processed_data_tibble$data, n = 5)),
                                   index = 1:length(run_var_5))
        
        did_it_flip <- negative_events > positive_events
        is_positive <- calculate_mean_differences$diff > 0
        if(did_it_flip == TRUE){
          is_positive <- ifelse(is_positive == TRUE, FALSE, TRUE)
        }
        
        if(is_shiny == T) setProgress(0.65, detail = 'Forward Ensemble')
        #played around with the numbers of where to 'chunk' out the data
        #art of balancing just the right amount of data to capture the transition
        #without getting too much to increase chance of wrong changepoint being detected
        #sometimes it seemed if event has a noisy spot the event start/end was detected
        #resulting in negative time ons or poor time on estimates
        
        forward_data <- tibble(s1_end = (regroup_data$state_1_end - 1)*conversion,
                               s2_end = (regroup_data$state_1_end + 1)*conversion)
        
        backwards_data <- tibble(s2_end = (regroup_data$state_2_end - 0)*conversion,
                                 s1_start = (regroup_data$state_2_end + 2)*conversion)
        
        
        ensemble_length <- 100 #data points, 25ms, 0.015 seconds
        better_time_on_starts <- vector()
        forward_ensemble_average_data <- vector("list")
        ensemble_keep1 <- vector()
        fcp <- list()
        forward_plots <- list()
        
        better_time_on_stops <- vector()
        backwards_ensemble_average_data <- vector("list")
        ensemble_keep2 <- vector()
        bcp <- list()
        backwards_plots <- list()
        
        better_displacements <- vector()
        
        for(c in 1:nrow(forward_data)){
          #print(c)
          #get event data chunk
          forward_chunk <- processed_data_tibble[forward_data$s1_end[[c]] : forward_data$s2_end[[c]],]
          forward_ensemble_chunk <- run_var_ensemble[forward_data$s1_end[[c]] : forward_data$s2_end[[c]],]
          
          backwards_chunk <- processed_data_tibble[backwards_data$s2_end[[c]] : backwards_data$s1_start[[c]],]
          backwards_ensemble_chunk <- run_var_ensemble[backwards_data$s2_end[[c]] : backwards_data$s1_start[[c]],]
          
          #if chunk has na values skip
          #this should be rare
          #mostly if the last event does not have enough state1 data after it
          #doing this helped avoid errors
          has_na <- table(is.na(forward_chunk$data))
          has_na2 <- table(is.na(backwards_chunk$data))
          if(length(has_na) > 1 | length(has_na2) > 1){
            better_time_on_starts[[c]] <- NA
            better_time_on_stops[[c]] <- NA
            ensemble_keep1[[c]] <- FALSE
            ensemble_keep2[[c]] <- FALSE
            next
          }
          
          #changepoint on transition into events
          forward_cpt_obj <- changepoint::cpt.mean(forward_ensemble_chunk$run_var_5, method = "AMOC")
          event_on <- changepoint::cpts(forward_cpt_obj)
          
          #changepoint on transition out of events
          backwards_cpt_obj <- changepoint::cpt.mean(backwards_ensemble_chunk$run_var_5, method = "AMOC")
          event_off <- changepoint::cpts(backwards_cpt_obj)
          
          # visualize the changepoint on raw data and running var
          # forward_plots[[c]] <- ggplot()+
          #   geom_point(aes(x = 1:nrow(forward_chunk), y = forward_chunk$data), color = 'gold')+
          #   geom_line(aes(x = 1:nrow(forward_chunk), y = forward_chunk$data), color = 'gold')+
          #   geom_point(aes(x = event_on, y = forward_chunk$data[event_on]), color = 'lawngreen', shape = 1, size = 2, stroke = 2)+
          #   ylab('nm')+
          #   xlab('DPs')+
          #   ggtitle('Forward')+
          #   lasertrapr::theme_black()
          # 
          # fe <- ggplot()+
          #   geom_point(aes(x = 1:nrow(forward_ensemble_chunk), y = forward_ensemble_chunk$run_var_5), color = 'gold')+
          #   geom_point(aes(x = 1:nrow(forward_ensemble_chunk), y = forward_ensemble_chunk$run_var_5), color = 'gold')+
          #   geom_point(aes(x = event_on, y = forward_ensemble_chunk$run_var_5[event_on]), color = 'lawngreen', shape = 1, size = 2, stroke = 2)+
          #   ylab('nm')+
          #   xlab('DPs')+
          #   ggtitle('Forward')+
          #   lasertrapr::theme_black()
          # 
          # backwards_plots[[c]] <- ggplot()+
          #   geom_point(aes(x = 1:nrow(backwards_chunk), y = backwards_chunk$data), color = 'gold')+
          #   geom_line(aes(x = 1:nrow(backwards_chunk), y = backwards_chunk$data), color = 'gold')+
          #   geom_point(aes(x = event_off, y = backwards_chunk$data[event_off]), color = 'lawngreen', shape = 1, size = 2, stroke = 2)+
          #   ylab('nm')+
          #   xlab('DPs')+
          #   ggtitle('Backwards')+
          #   lasertrapr::theme_black()
          # 
          # 
          # be <- ggplot()+
          #   geom_point(aes(x = 1:nrow(backwards_ensemble_chunk), y = backwards_ensemble_chunk$run_var_5), color = 'gold')+
          #   geom_point(aes(x = 1:nrow(backwards_ensemble_chunk), y = backwards_ensemble_chunk$run_var_5), color = 'gold')+
          #   geom_point(aes(x = event_off, y = backwards_ensemble_chunk$run_var_5[event_off]), color = 'lawngreen', shape = 1, size = 2, stroke = 2)+
          #   ylab('nm')+
          #   xlab('DPs')+
          #   ggtitle('Backwards')+
          #   lasertrapr::theme_black()
          # 
          # 
          # 
          # gridExtra::grid.arrange(forward_plots[[c]], backwards_plots[[c]])
          
          
          #if no changepoint id'd skip
          if(identical(event_on, numeric(0)) == TRUE){
            better_time_on_starts[[c]] <- NA
            ensemble_keep1[[c]] <- FALSE
          } else {
            #or record change point index
            cp_start <- forward_chunk$index[event_on]
            better_time_on_starts[[c]] <- cp_start
            ensemble_keep1[[c]] <- TRUE
            fcp[[c]] <- forward_cpt_obj
          }
          
          #do same for backwards
          if(identical(event_off, numeric(0)) == TRUE){
            better_time_on_stops[[c]] <- NA
            ensemble_keep2[[c]] <- FALSE
            
          } else {
            
            #or record the index where this occurred in the previous attempt
            cp_off <- backwards_chunk$index[event_off]
            better_time_on_stops[[c]] <- cp_off
            ensemble_keep2[[c]] <- TRUE
            bcp[[c]] <-  backwards_cpt_obj
            
          }
          if(identical(event_on, numeric(0)) == TRUE | identical(event_off, numeric(0)) == TRUE){
            next
          }
          
          
          #find length of event
          length_of_event <- nrow(processed_data_tibble[cp_start:(cp_off-1),])
          
          #take first 25ms for front and back ensemble
          
          if(length_of_event >= 100){
            #take first 15ms of event starting from the newly found start of event
            forward_25 <- tibble(data = processed_data_tibble$data[ cp_start : (cp_start + 99) ],
                                 ensemble_index = 0:(length(data)-1),
                                 event = c)
          } else {
            
            #take average step size between 5-6ms into event. ignore first 5 to assume you are in an event.
            front_avg <- mean(processed_data_tibble$data[(cp_start+25):(cp_start+30)])
            
            time_diff <- 100-length_of_event
            extend_event <- c(processed_data_tibble$data[ cp_start : (cp_off-1) ], rep(front_avg, time_diff))
            
            forward_25 <- tibble(data = extend_event,
                                 ensemble_index = 0:(length(data)-1),
                                 event = c)
          }
          
          
          before_event <- tibble(data = processed_data_tibble$data[ (cp_start - 75) : (cp_start - 1) ],
                                 ensemble_index = -length(data):-1,
                                 event = c)
          
          
          forward_25 %<>% rbind(before_event) %>% 
            dplyr::mutate(is_positive = is_positive[[c]],
                   direction = 'forward') %>%
            dplyr::arrange(ensemble_index)
          
          forward_ensemble_average_data[[c]] <- forward_25
          
          
          
          
          #take last 25ms of event starting from the newly found start of event
          #start indexing at 100 so we can eventually rbind forward and backwards and have a
          #0-149 indexed trace lining up two  25ms forward and backward ensembles
          
          if(length_of_event >= 100){
            #take first 15ms of event starting from the newly found start of event
            backwards_25 <- tibble(data = processed_data_tibble$data[ (cp_off-99) : cp_off ],
                                   ensemble_index = 100:199,
                                   event = c)
          } else {
            #take average step size between 5-6ms into event. ignore first 5 to assume you are in an event.
            back_avg <- mean(processed_data_tibble$data[(cp_off-25):(cp_off-30)])
            
            time_diff <- 100-length_of_event
            
            extend_event_back <- c(rep(back_avg, time_diff), processed_data_tibble$data[ cp_start : (cp_off-1) ])
            
            backwards_25 <- tibble::tibble(data = extend_event_back,
                                   ensemble_index = 100:199,
                                   event = c)
          }
          
          after_event <- tibble::tibble(data = processed_data_tibble$data[ (cp_off) : (cp_off + 74) ],
                                ensemble_index = 200:274,
                                event = c)
          
          backwards_25 %<>% rbind(after_event) %>%
            mutate(is_positive = is_positive[[c]],
                   direction = 'backwards') %>%
            arrange(ensemble_index)
          
          backwards_ensemble_average_data[[c]] <- backwards_25
          
          #get better displacements 
          exact_event_chunk <- processed_data_tibble$data[cp_start : cp_off]
          percent10 <- length_of_event*0.1
          better_displacements[[c]] <-  mean( processed_data_tibble$data[ (cp_start + percent10) : (cp_off - percent10) ])
          
          
        }
      
        
        better_time_on <- tibble(start = better_time_on_starts,
                                 stop = better_time_on_stops,
                                 keep1 = ensemble_keep1,
                                 keep2 = ensemble_keep2,
                                 n_event = 1:length(start),
                                 is_positive = is_positive) %>%
          mutate(better_time_on_dp = stop - start,
                 better_time_on_ms = (better_time_on_dp/5000)*1000)
        
        
        
        dygraph_periods <- cbind(better_time_on, regroup_data) %>%
          mutate(state_2_start = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                        state_1_end*conversion,
                                        start),
                 state_2_stop = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                       state_2_end*conversion,
                                       stop)) %>%
          rename("run_from" = state_1_end,
                 "run_to" = state_2_end) %>%
          dplyr::select(state_2_start, state_2_stop, run_from, run_to)
        
        
        
        
        ###add better on times & displacements to final table
        measured_events %<>% full_join(better_time_on) %>%
          mutate(final_time_ons_ms = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE | better_time_on_dp <= 0,
                                            time_on_ms,
                                            better_time_on_ms),
                 final_displacements = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE | better_time_on_dp <= 0,
                                              displacement_nm,
                                              better_displacements),
                 analyzer = 'hm-model') %>%
          dplyr::select(time_off_ms, final_time_ons_ms,  final_displacements, force, analyzer) %>%
          rename("time_on_ms" = final_time_ons_ms,
                 "displacement_nm" = final_displacements)
        
        
        
        ## SAVE OUTPUT ##
        if(is_shiny == T) setProgress(0.75, detail = 'Savings stuff')
        
        #save ensemble data
        forward_ensemble_df <- bind_rows(forward_ensemble_average_data)
        
        backwards_ensemble_df <- bind_rows(backwards_ensemble_average_data)
        
        ensemble_avg_df <- rbind(forward_ensemble_df, backwards_ensemble_df) %>%
          arrange(event)

        s1_avg_4plot <- tibble(state_order = seq(from = 1, length.out = length(state_1_avg), by = 2),
                               avg = flip_state_1_avg)
        
        
        s2_avg_4plot <- tibble(state_order = seq(from = 2, length.out = length(step_sizes), by = 2),
                               avg = measured_events$displacement_nm)
        
        
        hmm_overlay <- bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
          arrange(state_order)

        if(hmm_posterior$state[[length(hmm_posterior$state)]] == 2){
          
          overlay <- vector('list')
          for(i in seq_along(1:(nrow(hmm_overlay)-1))){
            
            overlay[[i]] <- rep(hmm_overlay$avg[i],
                                (conversion*rle_object$lengths[-length(rle_object$lengths)][i]))
          }
        } else {
          
          overlay <- vector('list')
          for(i in seq_along(1:nrow(hmm_overlay))){
            overlay[[i]] <- rep(hmm_overlay$avg[i],
                                (conversion*rle_object$lengths[-length(rle_object$lengths)][i]))
          }
        }
        
    overlay <- unlist(overlay)
        
     dy_rv <- tibble(window = 1:nrow(hmm_identified_events),
                        rv = hmm_identified_events$run_var)
     dy_rm <- tibble(Window = 1:nrow(hmm_identified_events),
                        rm = hmm_identified_events$run_mean)
     shades_df <- data.frame(start = dygraph_periods$run_from,
                                stop = dygraph_periods$run_to)
        
     shade_col <- '#E2E2E2'
     
     color <- c(pink(), purple())
          
     rv_dy <- dygraphs::dygraph(dy_rv, group = 'group') %>%
                dygraphs::dySeries('rv', color = color[[1]], strokeWidth = 2) %>%
                dygraphs::dyAxis('x', axisLineColor = '#FFFFFF', drawGrid = FALSE, axisLabelColor = '#FFFFFF') %>%
                dygraphs::dyAxis('y', label = 'Running Variance', drawGrid = FALSE,) %>%
                add_shades(periods = shades_df, color = shade_col) %>%
                dygraphs::dyUnzoom()

          
     rm_dy <- dygraphs::dygraph(dy_rm, group = 'group') %>%
                dygraphs::dySeries('rm', color = color[[2]],  strokeWidth = 2) %>%
                dygraphs::dyAxis('x', label = 'Window', drawGrid = FALSE) %>%
                dygraphs::dyAxis('y', label = 'Running Mean (nm)', drawGrid = FALSE) %>%
                add_shades(periods = shades_df, color = shade_col) %>%
                dygraphs::dyRangeSelector(fillColor ='white', strokeColor = color[[1]])
     
     if(is_shiny == T) setProgress(0.77, detail = "Plotting something...")
     d <- data.frame(index = (1:length(overlay)/5000),
                    raw = flip_raw[1:length(overlay)],
                    model = overlay)
  
    periods_df <- data.frame(start = dygraph_periods$state_2_start/5000,
                             stop = dygraph_periods$state_2_stop/5000)
   
    pni <- ((peak_nm_index * conversion) - 75)/5000
      
   overlay_dy <-  dygraphs::dygraph(d) %>% #raw_data_dygraph
                    dygraphs::dySeries('raw', color = '#242424', strokeWidth = 2) %>%
                    dygraphs::dySeries('model', color = pink(),  strokeWidth = 2) %>%
                    dygraphs::dyRangeSelector(fillColor ='white', strokeColor = color[[1]]) %>%
                    add_shades(periods_df, color = '#ffd2df') %>% #raw_periods
                    add_labels_hmm(measured_events, peak_nm_index = pni, labelLoc = 'bottom') %>% #results$events
                    dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
                    dygraphs::dyAxis('y', label = 'nm') %>%
                    dygraphs::dyUnzoom()

 
    #
    #
    # Plots of the running mean vs. running variance.
    # This provides insight into how the model divided data into either the baseline or event populations.
    # echo=FALSE, message = FALSE, fig.width = 16, fig.height = 6
   
   if(did_it_flip == FALSE){
    mean_var_tib <- tibble(rm = hmm_identified_events$run_mean,
                           rv = hmm_identified_events$run_var,
                           state = paste('State', hmm_identified_events$state))
   } else {
     mean_var_tib <- tibble(rm = hmm_identified_events$run_mean*-1,
                            rv = hmm_identified_events$run_var,
                            state = paste('State', hmm_identified_events$state))
   }
    
    
    mv1 <- ggplot2::ggplot(mean_var_tib)+
      geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
      scale_color_manual(values = c(pink(), purple()))+
      ggtitle('Mean-Variance (overlayed)')+
      ylab('Running Variance (nm)')+
      xlab('Running Mean (nm)')+
      theme_black(base_size = 18)+
      theme(legend.position = 'none')
     
    mv2 <- ggplot(mean_var_tib)+
      geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
      scale_color_manual(values = c(pink(), purple()))+
      facet_wrap(~state)+
      ggtitle('Mean-Variance (by state)')+
      ylab('')+
      xlab('Running Mean (nm)')+
      theme_black(base_size = 18)
    
    mv_state_plot <- gridExtra::grid.arrange(mv1, mv2, nrow = 1)
    
    if(is_shiny == T) setProgress(0.8, detail = "Calculating event frequency")


        
      
    
        report_data <- "event_freq_fail"
        #### EVENT FREQUENCY ####
        event_freq <- event_frequency(processed_data, rle_object, conversion)
                 
        
        results_data <- list(events = measured_events,
                             ensemble_avg = ensemble_avg_df,
                             event_freq = event_freq$data)
        
    
        if(is_shiny == T) setProgress(0.9, detail = "Saving Data")            
          
        report_data <- "success"
        obs_trap_data <- trap_data_rds[folder, ] %>% 
          dplyr::mutate(results = list(results_data),
                        report = report_data,
                        status = 'analyzed',
                        analyzer = 'hmm/changepoint')
        
        
       saveRDS(obs_trap_data, file = trap_data_rds$rds_file_path[[folder]])
        
       if(is_shiny == T) setProgress(0.95, detail = "Saving Viz")    
        
       plot_data <- list( events = measured_events,
                           peak_nm_index = (peak_nm_index * conversion)/5000,
                           shades_df = shades_df,
                           raw_periods = periods_df, 
                           event_freq_plot = event_freq$plot,
                           s2n = var_signal_to_noise,
                           mv_state_plot = mv_state_plot,
                           hmm_fit = hmm_fit,
                           rv_dy = rv_dy,
                           rm_dy = rm_dy,
                          hm_model_data = hmm_identified_events,
                           overlay_dy = overlay_dy )
        
                
        viz <- trap_data_rds[folder, ] %>% 
          dplyr::select(project, conditions, date, obs) %>%  
          dplyr::mutate(plot = list(plot_data))
        
        viz_path <- str_replace(trap_data_rds$rds_file_path[[folder]], 'trap-data.rds', 'viz.rds')
        saveRDS(viz, file = viz_path)
        #summary(hmm_fit) will gve hmm model == sum_fit object
        #measured events
        
        if(is_shiny == T) setProgress(0.97, 'Done with this one')
        

      }, error=function(e){
        showNotification(paste0("Analysis error in ",
                                trap_data_rds$date[[folder]],
                                " ",
                                trap_data_rds$conditions[[folder]],
                                " ",
                                trap_data_rds$obs[[folder]],
                                ". Error Message: ",
                                as.character(e)), type = 'warning', duration = NULL)
        writeLines(paste0("Analysis error in ",
                          trap_data_rds$date[[folder]],
                          " ",
                          trap_data_rds$conditions[[folder]],
                          " ",
                          trap_data_rds$obs[[folder]],
                          ". Error Message: ",
                          as.character(e)), error_file)
      })
      
    }
    
    close(error_file)
    if(is_shiny == T) setProgress(1, detail = "Done!")
    
}






#' Event Frequency
#' @noRd
#' @param processed_data A vector of processed trap data
#' @param rle_object A run-length-encoding object
#' @param conversion The conversion between running window time and 5kHz

event_frequency <- function(processed_data, rle_object, conversion){
  #
   
  #return a dataframe of event frequencies per each second
  #get the number of seconds in the trace
  seconds_in_trace <- length(processed_data)/5000
  
  #get the start indices for each event
  #events are id'd by HM-Model in running window length/time
  #need to convert those indices back to 5kHz time
  
  #remove the last row if trace ends in state 1
  # because there is no event after the last state 1
  
  
  start_event <- rle_object %>%
    dplyr::mutate(cumsum = cumsum(lengths),
                  start_event = (cumsum + 1)*round(conversion)) %>%
    dplyr::filter(values == 1) %>%
    head(-1) %>%
    dplyr::pull(start_event)
  
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
  
  
  
  g1 <- ggplot(find_it, aes(x = second, y = freq_start))+
    geom_line(aes(group = 1), color = pink())+
    geom_point(color = pink())+
    scale_x_continuous('', breaks = seq(0, nrow(find_it), by = 10))+
    ylab('Events Start')+
    theme_black(base_size = 16)
  
  g2 <- ggplot(find_it_end, aes(x = second, y = freq_stop))+
    geom_line(aes(group = 1) , color = purple())+
    geom_point(color = purple())+
    scale_x_continuous('', breaks = seq(0, nrow(find_it_end), by = 10))+
    ylab('Events End')+
    theme_black(base_size = 16)
    
  
  find_it$freq_stop <- find_it_end$freq_stop
  find_it$diff <- find_it$freq_start - find_it_end$freq_stop
  
  
  g3 <- ggplot(find_it, aes(x = second, y = diff))+
    geom_line(aes(group = 1), color = green() )+
    geom_point(color = green() )+
    scale_x_continuous('Seconds', breaks = seq(0, nrow(find_it), by = 10))+
    ylab('Diff')+
    theme_black(base_size = 16)
  
  
 g <- gridExtra::grid.arrange(g1, g2,g3, ncol = 1)
  
  

  list(data = find_it,
       plot = g)
       
}
