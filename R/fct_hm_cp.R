
#' Hidden markov analysis
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.


hidden_markov_changepoint_analysis <- function(trap_data, f, hz = 5000, w_width = 150, em_random_start, is_shiny = F, ...){
  # path <- "~/lasertrapr/project_myoV-phosphate/myoV-S217A_pH-7.0_30mM-Pi/2019-02-27/obs-01"
  # files <- list_files(path, pattern = 'trap-data.csv')
  # trap_data <- purrr::map_df(files$path, vroom::vroom)
  # trap_data %<>% nest(data = c(raw_bead, processed_bead, hm_overlay))
  # em_random_start <- FALSE
  # hz <- 5000
  # w_width <- 150
  #mv2nm <- trap_data$mv2nm
  nm2pn <- unique(trap_data$nm2pn)
  project <- unique(trap_data$project)
  conditions <- unique(trap_data$conditions)
  date <- unique(trap_data$date)
  obs <- unique(trap_data$obs)
  include <- unique(trap_data$include)
  if(!include){
    obs_trap_data_exit <- trap_data  %>%
      dplyr::mutate(report = 'user-excluded',
                    analyzer = 'none',
                    review = F)

    vroom::vroom_write(obs_trap_data_exit, path = file.path('~', 'lasertrapr', project, conditions, date, obs, 'trap-data.csv'), delim = ",")
    stop("User Excluded")
  }
 report_data  <- "error"
  error_file <- file(file.path(f$date$path, "error-log.txt"), open = "a")
      tryCatch({
        
        not_ready <- is_empty(trap_data$processed_bead)
        if(not_ready){
          if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
          stop('Data not processed')
          }
                  
        if(is_shiny) setProgress(0.05, paste("Analyzing", conditions, obs))
        
     
        if(is_shiny){
          defend_if_empty(trap_data$processed_bead, ui = paste0(obs, ' data not processed.'), type = 'error')
        }
  
        processed_data <- trap_data$processed_bead

        
        #### RUNNING MEAN & VAR ####
        
        if(is_shiny) setProgress(0.1, detail = "Calculating Running Windows")
        run_mean <- na.omit(RcppRoll::roll_meanl(processed_data, n = w_width, by = w_width/2))
        #run_mean_to_subtract <- na.omit(RcppRoll::roll_meanl(processed_data, n = 50))
        #processed_mean_removed <- processed_data[1:length(run_mean_to_subtract)] - run_mean_to_subtract
        run_var <- na.omit(RcppRoll::roll_varl(processed_data, n = w_width, by = w_width/2))
      
       
        #### HMM ####
        if(is_shiny) setProgress(0.25, detail = "HM-Model")
        
        hm_model_results <- fit_hm_model(trap_data = trap_data, 
                                         run_mean = run_mean, 
                                         run_var = run_var, 
                                         em_random_start = em_random_start,
                                         is_shiny = F, 
                                         project = project, 
                                         conditions = conditions, 
                                         date = date, 
                                         obs = obs)
        
        #### MEASURE EVENTS ####
        conversion <- w_width/2
        if(is_shiny) setProgress(0.5, detail = "Measuring")
        
        measured_hm_events <- measure_hm_events(processed_data = processed_data, 
                                                hm_model_results = hm_model_results, 
                                                conversion = conversion, 
                                                hz = hz,
                                                nm2pn = nm2pn)
        #### CHANGEPOINT ####
        if(is_shiny) setProgress(0.75, detail = "Changepoint")
        
         cp_data <- changepoint_and_ensemble(measured_hm_events = measured_hm_events, 
                                             hz = hz, 
                                             conversion = conversion)
                                            
      
     
        #add better on times & displacements to final table
        single_molecule_results <- measured_hm_events$measured_events_hm_estimates %>% 
          dplyr::full_join(cp_data$cp_event_transitions) %>%
          dplyr::mutate(final_time_ons_ms = ifelse(is.na(start) | is.na(stop)  | cp_time_on_dp <= 0,
                                            time_on_ms,
                                            cp_time_on_ms),
                 final_displacements = ifelse(is.na(start)  | is.na(stop)  | cp_time_on_dp <= 0,
                                              displacement_nm,
                                              displacements),
                 analyzer = 'hm-model/cp',
                 hm_event_start = measured_hm_events$hm_event_transitions$state_1_end + 1,
                 hm_event_stop = measured_hm_events$hm_event_transitions$state_2_end,
                 cp_event_start_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                        (hm_event_start - 1)*conversion,
                                        start),
                 cp_event_stop_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                       hm_event_stop*conversion,
                                       stop),
                 project = project,
                 conditions = conditions,
                 date = date, 
                 obs = obs, 
                 conversion = conversion,
                 peak_nm_index = (measured_hm_events$peak_nm_index * conversion - 75)/hz) %>%
          dplyr::select(project, 
                        conditions, 
                        date, 
                        obs, 
                        time_off_ms, 
                        final_time_ons_ms, 
                        final_displacements,
                        force, 
                        analyzer,
                        everything()) %>% 
          dplyr::select(-c(time_on_ms, displacement_nm)) %>% 
          dplyr::rename("time_on_ms" = final_time_ons_ms,
                 "displacement_nm" = final_displacements)   
        
       
       ####EVENT FREQ####
        if(is_shiny) setProgress(0.75, detail = "Event Frequency")
        
        event_freq <- event_frequency(processed_data, 
                                      measured_hm_events$viterbi_rle, 
                                      conversion, 
                                      hz = hz,
                                      ends_in_state_1 = measured_hm_events$ends_in_state_1)
        
        #get some data for plotting later
        s1_avg_4plot <- tibble::tibble(avg = measured_hm_events$state_1_avg,
                                       state_order = seq(from = 1, length.out = length(avg), by = 2))
        
        
        s2_avg_4plot <- tibble::tibble(avg = ifelse(is.na(cp_data$absolute_displacements), 
                                                    measured_hm_events$state_2_avg, 
                                                    cp_data$absolute_displacements),
                                      state_order = seq(from = 2, length.out = length(avg), by = 2))
        
        hmm_overlay <- bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
          arrange(state_order)

        overlay <- unlist(map2(hmm_overlay$avg,
                               measured_hm_events$viterbi_rle$lengths,
                               ~rep(.x, times = conversion * .y)))
        
        overlay <- c(overlay, rep(overlay[length(overlay)], length(processed_data) - length(overlay)))
        
        
        if(measured_hm_events$did_it_flip) hm_model_results %<>% mutate(run_mean = run_mean * -1)
  
        report_data  <- "success"
        
        trap_data %<>% 
          dplyr::mutate(processed_bead =  measured_hm_events$flip_raw,
                        hm_overlay = overlay,
                        report = report_data,
                        hz = hz, 
                        analyzer = 'hm/cp',
                        status = 'analyzed')
        
        if(is_shiny == T) setProgress(0.95, detail = 'Saving Data')
        file_names <-  c('trap-data.csv', 
                         'measured-events.csv',
                         'ensemble-data.csv',
                         'hm-model-data.csv',
                         'event-frequency.csv')
        
        file_paths <-  file.path('~', 'lasertrapr', project,  conditions, date, obs, file_names)
      
        data_to_save <- list(trap_data,
                             single_molecule_results,
                             cp_data$ensemble_data,
                             hm_model_results,
                             event_freq)
        
        purrr::walk2(data_to_save, file_paths, ~vroom::vroom_write(x = .x, path = .y, delim = ','))
        

      }, error=function(e){
        showNotification(paste0("Analysis error in ",
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
      })
    
    close(error_file)
    if(is_shiny == T) setProgress(1, detail = "Done!")
      
    return(invisible())
}
    
   
    






