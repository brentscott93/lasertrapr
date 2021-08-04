
#' Hidden markov analysis
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.
#' 
hidden_markov_changepoint_analysis <- function(trap_data,
                                               f = f,
                                               hz = 5000,
                                               w_width = 150,
                                               w_slide = "1/2",
                                               use_channels,
                                               em_random_start, 
                                               front_cp_method,
                                               back_cp_method,
                                               cp_running_var_window,
                                               displacement_type = "avg",
                                               is_shiny = F, 
                                               opt,
                                               ...){
 
  # dev_path <- '~/lasertrapr/project_myoV-phosphate/myoV-S217A_pH-7.0_30mM-Pi/2020-11-01/obs-19/trap-data.csv'
  # trap_data <- read_csv(dev_path)
  # w_width = 150
  # em_random_start = F
  #w_slide <- "1/2"
  #use_channels <- "Mean/Var"
  #hz <- 5000
  
  nm2pn <- unique(trap_data$nm2pn)
  project <- unique(trap_data$project)
  conditions <- unique(trap_data$conditions)
  date <- unique(trap_data$date)
  obs <- unique(trap_data$obs)
  include <- unique(trap_data$include)
  mv2nm <-  unique(trap_data$mv2nm)
  
  path <- file.path(path.expand("~"),
                    "lasertrapr", 
                    project,
                    conditions,
                    date,
                    obs, 
                    "trap-data.csv")
  
  if(is_shiny) setProgress(0.03, paste("Reading Data", conditions, obs))
  
  trap_data <- data.table::fread(path) 
  
  report_data  <- "error"
  error_file <- file(file.path(f$date$path, "error-log.txt"), open = "a")
      tryCatch({
        if(!include){
          obs_trap_data_exit <- trap_data  %>%
            dplyr::mutate(report = 'user-excluded',
                          analyzer = 'none',
                          review = F)
          
           data.table::fwrite(obs_trap_data_exit, 
                              file = file.path(path.expand("~"),
                                              "lasertrapr", 
                                              project,
                                              conditions,
                                              date,
                                              obs, 
                                              "trap-data.csv"),
                              sep = ",")
          stop("User Excluded")
        }
        
        
        not_ready <- is_empty(trap_data$processed_bead)
        if(not_ready){
          if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
          stop('Data not processed')
          }
                  
        if(is_shiny){
          setProgress(0.05, paste("Analyzing", conditions, obs))
          defend_if_empty(trap_data$processed_bead, ui = paste0(obs, ' data not processed.'), type = 'error')
        }
  
        processed_data <- trap_data$processed_bead

        
        #### RUNNING MEAN & VAR ####
        if(w_slide == "1-pt"){
          ws <- 1 
        } else if(w_slide == "1/4"){
          ws <- w_width*0.25
        } else if(w_slide == "1/2"){
          ws <- w_width*0.5
        } else if(w_slide == "3/4"){
          ws <- w_width*0.75
        } else if(w_slide == "No-overlap"){
          ws <- w_width
        }
        
        if(is_shiny) setProgress(0.1, detail = "Calculating Running Windows")
        
        run_mean <- na.omit(RcppRoll::roll_meanl(processed_data, n = w_width, by = ws))
        run_var <- na.omit(RcppRoll::roll_varl(processed_data, n = w_width, by = ws))
      
       
        #### HMM ####
        if(is_shiny) setProgress(0.25, detail = "HM-Model")
        
        hm_model_results <- fit_hm_model(trap_data = trap_data, 
                                         run_mean = run_mean, 
                                         run_var = run_var, 
                                         use_channels = use_channels,
                                         em_random_start = em_random_start,
                                         is_shiny = F, 
                                         project = project, 
                                         conditions = conditions, 
                                         date = date, 
                                         obs = obs)
        
        #### MEASURE EVENTS ####
        conversion <- ws
        if(is_shiny) setProgress(0.5, detail = "Measuring")
        
        measured_hm_events <- measure_hm_events(processed_data = processed_data, 
                                                hm_model_results = hm_model_results, 
                                                conversion = conversion, 
                                                hz = hz,
                                                nm2pn = nm2pn)
        #### CHANGEPOINT ####
        if(is_shiny) setProgress(0.75, detail = "Changepoint")
        
         cp_data <- changepoint_analysis(measured_hm_events = measured_hm_events, 
                                         hz = hz, 
                                         conversion = conversion,
                                         mv2nm = mv2nm, 
                                         conditions = conditions,
                                         front_cp_method = front_cp_method,
                                         back_cp_method = back_cp_method,
                                         cp_running_var_window = cp_running_var_window,
                                         ws = ws,
                                         displacement_type = displacement_type)
                                    
                                            
      
     
        #add better on times & displacements to final table
        single_molecule_results <- measured_hm_events$measured_events_hm_estimates %>% 
          dplyr::full_join(cp_data$cp_event_transitions) %>%
          dplyr::mutate(final_time_ons_ms = ifelse(is.na(start) | is.na(stop)  | cp_time_on_dp <= 0,
                                                   time_on_ms,
                                                   cp_time_on_ms),
                         final_displacements = ifelse(is.na(start)  | is.na(stop)  | cp_time_on_dp <= 0,
                                                      displacement_nm,
                                                      cp_displacements),
                         analyzer = 'hm-model/cp',
                         hm_event_start = measured_hm_events$hm_event_transitions$state_1_end + 1,
                         hm_event_stop = measured_hm_events$hm_event_transitions$state_2_end,
                         cp_event_start_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                                (hm_event_start - 1)*conversion,
                                                start),
                         cp_event_stop_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                                               hm_event_stop*conversion,
                                               stop),
                         keep = ifelse(final_time_ons_ms <= 1, FALSE, keep),
                         project = project,
                         conditions = conditions,
                         date = date, 
                         obs = obs, 
                         conversion = conversion,
                         peak_nm_index = cp_data$displacement_mark,
                         event_user_excluded = FALSE) %>%
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
                                      ends_in_state_1 = measured_hm_events$ends_in_state_1,
                                      project = project, 
                                      conditions = conditions,
                                      date = date, 
                                      obs = obs)
        
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
        
        options_df <- as.data.frame(opt) %>% 
          dplyr::mutate(project = project,
                 conditions = conditions, 
                 date = date, 
                 obs = obs) %>% 
          dplyr::select(project, conditions, date, obs, everything())
        
        if(is_shiny == T) setProgress(0.95, detail = 'Saving Data')
        file_names <-  c('trap-data.csv', 
                         'measured-events.csv',
                         #'ensemble-data.csv',
                         'hm-model-data.csv',
                         'event-frequency.csv',
                         'options.csv')
        
        file_paths <-  file.path(path.expand("~"), "lasertrapr", project,  conditions, date, obs, file_names)
      
        data_to_save <- list(trap_data,
                             single_molecule_results,
                             #cp_data$ensemble_data,
                             hm_model_results,
                             event_freq,
                             options_df)
        
        purrr::walk2(data_to_save, file_paths, ~data.table::fwrite(x = .x, file = .y, sep = ","))
        

      }, error=function(e){
        if(!include){
          showNotification(paste0("Skipping ", obs, ' user excluded'), type = 'message', duration = 2)
        } else {
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
        }
      })
    
    close(error_file)
    if(is_shiny == T) setProgress(1, detail = "Done!")
      
    return(invisible())
}
    
   
    






