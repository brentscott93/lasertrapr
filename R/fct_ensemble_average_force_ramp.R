#' Prep Ensembles for Ensemble Average
#'
#' @param trap_selected_project a trap project
#' @param ms_extend_s2 ms
#' @param ms_extend_s1 ms
#' @param hz sampling freq.
#'
#' @return nothing. Saves ensemble-data.csv in each obs-## folder.
#' @import data.table
#' @noRd
prep_ensemble_force_ramp <- function(trap_selected_project,
                                     ms_extend_s2 = 3,
                                     ms_extend_s1 = 3,
                                     ms_2_skip,
                                     hz,
                                     is_shiny = TRUE,
                                     events_select){
  # trap_selected_project <- "~/lasertrapr/project_myoV-phosphate"
  # ms_extend_s2 = 3
  # ms_extend_s1 = 3
                                        # trap_selected_project <- "~/lasertrapr/project_myoV-phosphate"
                                        # ms_extend_s2 = 3
                                        # ms_extend_s1 = 3
  ## browser()

  if(is_shiny) incProgress(0.01, detail = "Removing Old Files")

  old_files <-
    lasertrapr:::list_files(
                   trap_selected_project,
                   pattern = "ensemble-data.csv",
                   recursive = TRUE)

  file.remove(old_files$path)

  old_files <-
    lasertrapr:::list_files(
                   trap_selected_project,
                   pattern = "ensemble-data.rds",
                   recursive = TRUE)

  file.remove(old_files$path)

  old_files <-
    lasertrapr:::list_files(
                   trap_selected_project,
                   pattern = "substeps.csv",
                   recursive = TRUE)

  file.remove(old_files$path)
                                        # trap_data_paths <-
                                        #   lasertrapr:::list_files(
                                        #     trap_selected_project,
                                        #     pattern = "trap-data.csv",
                                        #     recursive = TRUE)

  options_data_paths <-
    lasertrapr:::list_files(
                   trap_selected_project,
                   pattern = "options.csv",
                   recursive = TRUE)

  options_data <-
    data.table::rbindlist(
                  lapply(
                    options_data_paths$path,
                    data.table::fread),
                  fill = TRUE)

                                        # trap_data <-
                                        #   data.table::rbindlist(
                                        #     lapply(
                                        #       trap_data_paths$path,
                                        #       data.table::fread,
                                        #       nrows = 1),
                                        #     fill = TRUE)

  options_data <- options_data[report == "success" & review == TRUE & include == TRUE]

  n_channels <- unique(options_data$channels)

  event_file_paths <- file.path(path.expand("~"),
                                "lasertrapr",
                                options_data$project,
                                options_data$conditions,
                                options_data$date,
                                options_data$obs,
                                "measured-events.csv")

  event_files_filtered <-
    data.table::rbindlist(
                  lapply(
                    event_file_paths,
                    data.table::fread
                  ),
                  fill = TRUE #do i really want this?
                )


longest_event_df <-
  event_files_filtered[keep == TRUE & event_user_excluded == FALSE,
                       .(longest_event = max(cp_time_on_dp)),
                       by = conditions]

  dp_extend_s2 <- ms_extend_s2*hz/1000
  dp_extend_s1 <- ms_extend_s1*hz/1000

  for(i in seq_len(nrow(options_data))){
    print(paste0("i=", i))
    ## if(i==16) browser()
    if(is_shiny) incProgress(1/nrow(options_data), detail = paste0(i, " of ", nrow(options_data)))
    path <- file.path(path.expand("~"),
                      "lasertrapr",
                      options_data$project[[i]],
                      options_data$conditions[[i]],
                      options_data$date[[i]],
                      options_data$obs[[i]])
    trap_data_path <- file.path(path, "trap-data.csv")
    substeps_list <- vector("list")
    event_ensembles_list <- vector("list")
    running_average_list <- vector("list")


      trap_trace <- data.table::fread(trap_data_path, select = "processed_bead")[[1]]
      measured_events_path <- file.path(path, "measured-events.csv")
      measured_events <-  data.table::fread(measured_events_path,
                                            select = c("conditions",
                                                       "cp_event_start_dp",
                                                       "cp_event_stop_dp",
                                                       "keep",
                                                       "event_user_excluded",
                                                       "force"))

      measured_events$id <- 1:nrow(measured_events)
      measured_events <- measured_events[keep == TRUE & event_user_excluded == FALSE]


      measured_events[, event_length := (cp_event_stop_dp-cp_event_start_dp)+1]

      end_of_prior_event_dp <- c(1, head(measured_events$cp_event_stop_dp, -1))
      start_of_next_event_dp <- c(tail(measured_events$cp_event_start_dp, -1), length(trap_trace))
## browser()
    if(events_select == "positive"){
      measured_events <- measured_events[force >= 0]
    } else if(events_select == "negative"){
      measured_events <- measured_events[force <= 0]
  }

      event_ensembles <- list()
      running_average_f <- vector()
      running_average_b <- vector()
      for(event in seq_len(nrow(measured_events))){
                                        #print(paste0("event=", event))
        start <- measured_events$cp_event_start_dp[[event]]
        stop <- measured_events$cp_event_stop_dp[[event]]
        event_chunk <- trap_trace[start:stop]
        longest_event <- longest_event_df[which(longest_event_df$conditions == measured_events$conditions[[event]]),]
        longest_event <- longest_event$longest_event
        if(length(event_chunk) == longest_event){
          forward_event <- data.table(data = event_chunk,
                                      ensemble_index = 0:(longest_event-1))
          backwards_event <- data.table(data = event_chunk,
                                        ensemble_index = -(longest_event-1):0)
        } else {
          s2_avg <- mean(trap_trace[(stop - dp_extend_s2):stop])
          time_diff <- longest_event - length(event_chunk)
          forward_extended_event <- c(event_chunk, rep(s2_avg, time_diff))
          forward_event <- data.table(data = forward_extended_event,
                                      ensemble_index = 0:(longest_event-1))
          delay_s1_start <- ms_2_skip*hz/1000
          s1_total_time <- delay_s1_start+dp_extend_s1
          if(length(event_chunk) <= s1_total_time) next
          s1_avg <-  mean(event_chunk[(delay_s1_start:(delay_s1_start+dp_extend_s1))])
          backwards_extended_event <- c(rep(s1_avg, (time_diff+delay_s1_start)), event_chunk[-c(1:delay_s1_start)])
          backwards_event <- data.table(data = backwards_extended_event,
                                        ensemble_index = -(longest_event-1):0)
        }

        ms_10 <- 10*hz/1000
        end_forward_base <- start - 1
        if((end_forward_base - ((ms_10*2)-1) <= 0)){
          pad2do <- abs(end_forward_base - ((ms_10*2)-1))
          tt2 <- trap_trace[1:end_forward_base]
          ei2 <- -(ms_10*2):-1
          tt2 <- c(rep(tt2[1], pad2do), tt2)
          before_forwards <- data.table(data = tt2,
                                        ensemble_index = ei2)
        } else {
        before_forwards <- data.table(data = trap_trace[(end_forward_base - ((ms_10*2)-1)):end_forward_base],
                                      ensemble_index = -(ms_10*2):-1)
         }

        ms_1 <- 1*hz/1000
        ms_5 <- 3*hz/1000

        ## mean_baseline_prior <- mean( trap_trace[(end_forward_base - (ms_5-1)):(end_forward_base-ms_1)] )
        #get entire baseline mean before and after
        #end prior event
        epe <- end_of_prior_event_dp[[event]]
        mean_baseline_prior <- mean( trap_trace[epe:start] )

        #start next event
        sne <- start_of_next_event_dp[[event]]
        mean_baseline_after <- mean( trap_trace[stop:sne] )


        start_backwards_base <- stop + 1
        after_backwards <- data.table(data = trap_trace[start_backwards_base:(start_backwards_base + ((ms_10*2)-1))],
                                      ensemble_index = 1:(ms_10*2))


        ## mean_baseline_after <- mean( trap_trace[start_backwards_base:(start_backwards_base + (ms_5-1))] )

        forward_ensemble <- rbind(before_forwards, forward_event)
        forward_ensemble[,
                         `:=`(direction = "forward",
                              forward_backward_index = -(ms_10*2):(longest_event-1)
                              ## event_index = event
                              )
                         ## signal_ratio =  measured_events$front_signal_ratio[[event]])
                         ]

      ## ensemble <-  rbind(forward_ensemble, backwards_ensemble)
      ## ensemble[,
      ##          `:=`(project = options_data$project[[i]],
      ##               conditions = options_data$conditions[[i]],
      ##               date = options_data$date[[i]],
      ##               obs = options_data$obs[[i]],
      ##               bead = b,
      ##               fb_time = forward_backward_index/hz)
               ## ]

        backwards_ensemble <- rbind(backwards_event, after_backwards)
        backwards_ensemble[,
                           `:=`(direction = "backwards",
                                forward_backward_index = longest_event:((2*longest_event)+((ms_10*2)-1))
                                ## event_index = event
                                )
                           ## signal_ratio = measured_events$back_signal_ratio[[event]])
                           ]

        ## ensemble <-  rbind(forward_ensemble, backwards_ensemble)
        ## ensemble[,
        ##          `:=`(project = options_data$project[[i]],
        ##               conditions = options_data$conditions[[i]],
        ##               date = options_data$date[[i]],
        ##               obs = options_data$obs[[i]],
        ##               bead = b)
        ##          ]

        ## if(analyzer == "covar"){
        ##   forward_ensemble$data <- forward_ensemble$data - mean_baseline_prior
        ##   backwards_ensemble$data <- backwards_ensemble$data - mean_baseline_after
        ## }

      ## setorder(ensemble, cols = "forward_backward_index")

      ## if(downsample_ensemble_by != 1){
      ##   ds_pts <- seq(1, nrow(ensemble), by = downsample_ensemble_by)
      ##   ensemble <- ensemble[ds_pts]
      ## }

        if(event == 1){
       running_average_f <- forward_ensemble$data
       running_average_b <- backwards_ensemble$data
       } else {
       running_average_f <- running_average_f + forward_ensemble$data
       running_average_b <- running_average_b + backwards_ensemble$data
       }

      ## event_ensembles[[event]] <- ensemble

      ## substeps[[event]] <- data.table(
      ##   project = options_data$project[[i]],
      ##   conditions = options_data$conditions[[i]],
      ##   date = options_data$date[[i]],
      ##   obs = options_data$obs[[i]],
      ##   bead = b,
      ##   event_id = measured_events$id[[event]],
      ##   prior_unbound_position_nm = mean_baseline_prior,
      ##   bead_position_substep_1_nm = s1_avg,
      ##   substep_1_nm = s1_avg-mean_baseline_prior,
      ##   bead_position_substep_2_nm = s2_avg,
      ##   substep_2_nm = s2_avg-s1_avg,
      ##   total_step_nm = (s1_avg-mean_baseline_prior)+(s2_avg-s1_avg),
      ##   substep_2_nm_alt = (s2_avg-mean_baseline_after)-(s1_avg-mean_baseline_prior),
      ##   total_step_nm_alt = s2_avg - mean_baseline_after,
      ##   after_unbound_position_nm = mean_baseline_after
      ##)

      ## print(paste0("b = ", b, "id: ", measured_events$id[[event]], "; substep1: ", s1_avg, "; substep2: ", s2_avg))
    }

      ## running_average_f <- running_average_f/event
      ## running_average_b <- running_average_b/event

      forward_ensemble[, data := running_average_f]
      backwards_ensemble[, data := running_average_b]


      ensemble <-  rbind(forward_ensemble, backwards_ensemble)
      ensemble[,
               `:=`(project = options_data$project[[i]],
                    conditions = options_data$conditions[[i]],
                    date = options_data$date[[i]],
                    obs = options_data$obs[[i]])
               ]


}

    ensemble_path <- file.path(path, "ensemble-data.csv")
    data.table::fwrite(ensemble, file = ensemble_path, sep = ",")

    ensemble_options <- data.table::data.table( ms_extend_s2, ms_extend_s1, ms_2_skip, event)
    data.table::fwrite(ensemble_options, file = file.path(path, "options-prep-ensemble.csv"), sep = ",")

    ## substeps_path <- file.path(path, "substeps.csv")
    ## data.table::fwrite(substeps_list, file = substeps_path, sep = ",")

}








#' Ensemble Average
#' @param trap_selected_project full path to currently selected project
#' @param is_shiny logical. is function being used in shiny or not.
#' @noRd
#' @import data.table
avg_ensembles_force_ramp <- function(project, is_shiny = TRUE){
  #con <-lasertrapr:::list_dir(path = "~/lasertrapr/project_hitch-simulations")
  #project <- "project_hitch-simulations"
  ## browser()
  print("Starting Avg")
  project_path <- file.path(path.expand("~"), "lasertrapr", project)
  con <- list_dir(path = project_path)
  con <- con[grep("summary", con$name, invert = TRUE, ignore.case = TRUE),]$name

  ## ee_forward_data <- list()
  ## ee_backwards_data <- list()
  forward_backward <- vector("list")
  for(i in 1:length(con)){
    if(is_shiny) incProgress(1/(length(con)*2), detail = paste0("Reading ensembles from ", con[[i]]))

    ## forward_ee_paths <- list.files(path = file.path(project_path, con[[i]]),
    ##                        recursive = TRUE,
    ##                        full.names = TRUE,
    ##                        pattern = "forward-ensemble-data.csv")

    ## backwards_ee_paths <- list.files(path = file.path(project_path, con[[i]]),
    ##                        recursive = TRUE,
    ##                        full.names = TRUE,
    ##                        pattern = "backwards-ensemble-data.csv")

    ee_paths <- list.files(path = file.path(project_path, con[[i]]),
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = "ensemble-data.csv")


    ee_data <- lapply(ee_paths, ee_fread, is_shiny = is_shiny)
    ee_data <- data.table::rbindlist(ee_data)

    ee_options <- list.files(path = file.path(project_path, con[[i]]),
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = "options-prep-ensemble.csv")

    ee_opt_data <- lapply(ee_options, fread)
    ee_opt_data <- data.table::rbindlist(ee_opt_data)

    num_events <- sum(ee_opt_data$event)


    forward_backward[[i]] <- ee_data[,
      .(avg = sum(data)/num_events),
      by = .(conditions, direction, ensemble_index, forward_backward_index)]
}
  if(is_shiny) incProgress(1/(length(con)*2), detail = paste0("Combining all data", con[[i]]))
  ## ee_backwards_data <- data.table::rbindlist(ee_backwards_data)
  ## ee_forward_data <- data.table::rbindlist(ee_forward_data)
  ## forward_backward <- rbind(ee_forward_data, ee_backwards_data)
  forward_backward <- data.table::rbindlist(forward_backward)
  ## print("Completed Avg")
  ## str(forward_backward)

  summary_folder <- file.path(project_path, "summary")
  if(!dir.exists(summary_folder)){
    dir.create(summary_folder)
  }
  data.table::fwrite(forward_backward, #formerly called forward_backward
                     file.path(summary_folder, "ensemble-averages.csv")
                     )

  ## return(forward_backward)
  return(invisible())
}
