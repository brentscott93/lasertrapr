
#' Force balance analysis. This is exactly the same as 1 channel HM/CP except at the end. It reads in the other non-preferred channel and calculates force and subtracts.
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.
#'
force_balance_analysis <- function(trap_data,
                                   f = f,
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
  #browser()
  project <- unique(trap_data$project)
  conditions <- unique(trap_data$conditions)
  date <- unique(trap_data$date)
  obs <- unique(trap_data$obs)

  o_path <- file.path(path.expand("~"),
                    "lasertrapr",
                    project,
                    conditions,
                    date,
                    obs,
                    "options.csv")

  o <- data.table::fread(o_path)

  include <- o$include
  if(is.na(include)) include <- FALSE
  mv2nm <-  o$mv2nm
  nm2pn <- o$nm2pn
  hz <- o$hz
  if(is.null(o$channels)) o$channels <- 1
  ## if(is.na(o$channels)) o$channels <- 1
  channels <- o$channels

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
          obs_trap_data_exit <-
            o %>%
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
                                              "options.csv"),
                              sep = ",")
          stop("User Excluded")
        }

        if(channels == 1){
        not_ready <- rlang::is_empty(trap_data$processed_bead)
        } else {
        not_ready <- rlang::is_empty(trap_data$processed_bead_1)
        }
        if(not_ready){
          if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
          stop('Data not processed')
          }

        if(is_shiny){
          setProgress(0.05, paste("Analyzing", conditions, obs))
          if(channels == 1){
          defend_if_empty(trap_data$processed_bead, ui = paste0(obs, ' data not processed.'), type = 'error')
          } else {
          defend_if_empty(trap_data$processed_bead_1, ui = paste0(obs, ' data not processed.'), type = 'error')
          }
        }

        if(channels == 1){
          processed_data <- trap_data$processed_bead
        } else if(channels == 2){
          if(o$preferred_channel == 1){
            processed_data <- trap_data$processed_bead_1
          } else if(o$preferred_channel == 2){
            processed_data <- trap_data$processed_bead_2
            mv2nm <- o$mv2nm2
            nm2pn <- o$nm2pn2
          }
        }

        #### RUNNING MEAN & VAR ####
        if(w_slide == "1-Pt"){
          ws <- 1
        } else if(w_slide == "1/4"){
          ws <- round_any(w_width*0.25, 1)
        } else if(w_slide == "1/2"){
          ws <- round_any(w_width*0.5, 1)
        } else if(w_slide == "3/4"){
          ws <- round_any(w_width*0.75, 1)
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
                                                nm2pn = nm2pn,
                                                force_balance = TRUE)
        #### CHANGEPOINT ####
        if(is_shiny) setProgress(0.75, detail = "Changepoint")

         cp_data <- changepoint_analysis(measured_hm_events = measured_hm_events,
                                         hz = hz,
                                         conversion = conversion,
                                         mv2nm = mv2nm,
                                         nm2pn = nm2pn,
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
                         final_forces = ifelse(is.na(start)  | is.na(stop)  | cp_time_on_dp <= 0,
                                                      force,
                                                      cp_forces),
                         analyzer = 'force_balance',
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
                        final_forces,
                        analyzer,
                        everything()) %>%
          dplyr::select(-c(time_on_ms, displacement_nm, force)) %>%
          dplyr::rename("time_on_ms" = final_time_ons_ms,
                 "displacement_nm" = final_displacements,
                 "force_pn" = final_forces)

################################################
################################################
################################################
# FORCE BALANCE NEW SECTION
## browser()
        if(o$channels == 1) stop("This dataset only has 1 channel. Force balance requires 2.")

        if(o$preferred_channel == 1){
          other_processed_data <- trap_data$processed_bead_2
          other_nm2pn <- o$nm2pn2
        } else if(o$preferred_channel == 2){
          other_processed_data <- trap_data$processed_bead_1
          other_nm2pn <- o$nm2pn1
        }
        #apply direction correction to other bead
        ## if(measured_hm_events$did_it_flip){
        ##   other_processed_data <- other_processed_data*-1
        ## }

        other_displacements_nm <- vector()
        other_start_vec <- vector()
        other_stop_vec <- vector()
        for(r in seq_len(nrow(single_molecule_results))){
          other_ref_point <- single_molecule_results$peak_nm_index[[r]]
          # peak nm index is in seconds, get a 5ms window and convert to dp time
          other_start <- (other_ref_point - (4/1000))*hz
          other_stop <- (other_ref_point + (1/1000))*hz
          other_chunk_mean <- mean(other_processed_data[other_start:other_stop])

          other_start_vec[[r]] <- other_start
          other_stop_vec[[r]] <- other_stop

          if(r == 1){
            other_baseline_range <- 1:single_molecule_results$cp_event_start_dp[[r]]
          } else {
            other_current <- single_molecule_results$cp_event_start_dp[[r]]
            other_previous <- single_molecule_results$cp_event_stop_dp[[(r-1)]]
            other_baseline_range <- other_previous:other_current
          }

          other_baseline_mean <- mean(other_processed_data[other_baseline_range])

          other_displacement <- other_chunk_mean - other_baseline_mean

          other_displacements_nm[[r]] <- other_displacement
        }

        single_molecule_results$other_displacement_nm <- other_displacements_nm
        single_molecule_results$other_force_pn <- other_displacements_nm*other_nm2pn
        single_molecule_results$other_peak_window_start_dp <- other_start_vec
        single_molecule_results$other_peak_window_stop_dp <- other_stop_vec


        single_molecule_results <- single_molecule_results %>%
          dplyr::mutate(force_balance_pn = force_pn - other_force_pn)

# ##############################################
# ##############################################
################################################
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

        hmm_overlay <- dplyr::bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
          dplyr::arrange(state_order)

        overlay <- unlist(purrr::map2(hmm_overlay$avg,
                               measured_hm_events$viterbi_rle$lengths,
                               ~rep(.x, times = conversion * .y)))

        overlay <- c(overlay, rep(overlay[length(overlay)], length(processed_data) - length(overlay)))

        if(measured_hm_events$did_it_flip) hm_model_results <- dplyr::mutate(hm_model_results, run_mean = run_mean * -1)

        report_data  <- "success"

        trap_data <-
          dplyr::mutate(trap_data,
                        processed_bead =  measured_hm_events$flip_raw,
                        other_processed_bead = other_processed_data,
                        hm_overlay = overlay)

       ## browser()
        opt_df <- as.data.frame(opt)

                                        # option cols to keep is a terrible name
                                        # its the columns names to keep, SO they can be removed
        options_cols_to_keep <- names(opt_df) %in% names(o)
        options_cols_to_keep <- names(opt_df)[options_cols_to_keep]

        options_df <-
          o %>%
          dplyr::select(-c(options_cols_to_keep)) %>%
          cbind(opt_df) %>%
          dplyr::mutate( analyzer = 'force_balance',
                        status = 'analyzed',
                        report = report_data,) %>%
          dplyr::select(project, conditions, date, obs, everything())
        ## } else {
        ##   options_df <-
        ##     o %>%
        ##     cbind(opt_df) %>%
        ##     dplyr::mutate( analyzer = 'hm/cp',
        ##                    status = 'analyzed',
        ##                    report = report_data,) %>%
        ##     dplyr::select(project, conditions, date, obs, everything())
        ## }


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
