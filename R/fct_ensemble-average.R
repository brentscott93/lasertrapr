#' Prep Ensembles for Ensemble Average
#'
#' @param trap_selected_project 
#' @param ms_extend_s2 
#' @param ms_extend_s1 
#' @param hz 
#'
#' @return nothing. Saves ensemble-data.csv in each obs-## folder.
#' @import data.table
#' @noRd
prep_ensemble <- function(trap_selected_project,
                          ms_extend_s2 = 3, 
                          ms_extend_s1 = 3, 
                          ms_2_skip,
                          hz,
                          is_shiny = TRUE,
                          tmin_ms,
                          analyzer,
                          downsample_ensemble_by){
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

  if(is.null(n_channels)) n_channels <- 1
  if(n_channels == 2){
    event_files_filtered[, keep_add := keep_1+keep_2]
    event_files_filtered[, keep := ifelse(keep_add == 2, TRUE, FALSE)]

    longest_event_df <-
      event_files_filtered[keep == TRUE & event_user_excluded == FALSE,
                           .(longest_event = max(c(attachment_duration_bead_1_dp, attachment_duration_bead_2_dp))),
                           by = conditions]
  } else {

    longest_event_df <-
      event_files_filtered[keep == TRUE & event_user_excluded == FALSE,
                           .(longest_event = max(cp_time_on_dp)),
                           by = conditions]
  }
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
    for(b in seq_len(n_channels)){
      if(n_channels == 1){
        trap_trace <- data.table::fread(trap_data_path, select = "processed_bead")$processed_bead
        measured_events_path <- file.path(path, "measured-events.csv")
        measured_events <-  data.table::fread(measured_events_path,
                                              select = c("conditions",
                                                         "cp_event_start_dp",
                                                         "cp_event_stop_dp",
                                                         "keep",
                                                         ## "front_signal_ratio",
                                                         ## "back_signal_ratio",
                                                         ## "is_positive",
                                                         "event_user_excluded"))

        measured_events$id <- 1:nrow(measured_events)
        measured_events <- measured_events[keep == TRUE & event_user_excluded == FALSE]
      } else {
        trap_trace <- data.table::fread(trap_data_path, select = paste0("processed_bead_", b))[[1]]

        measured_events_path <- file.path(path, "measured-events.csv")

        measured_events <-  data.table::fread(measured_events_path,
                                              select = c("conditions",
                                                         paste0("cp_event_start_dp_", b),
                                                         paste0("cp_event_stop_dp_", b),
                                                         paste0("keep_", b),
                                                         "event_user_excluded"))

        measured_events$id <- 1:nrow(measured_events)
        if(b == 1){
          measured_events <- measured_events[keep_1 == TRUE & event_user_excluded == FALSE]
          measured_events$cp_event_start_dp <- measured_events$cp_event_start_dp_1
          measured_events$cp_event_stop_dp <- measured_events$cp_event_stop_dp_1
        } else {
          measured_events <- measured_events[keep_2 == TRUE & event_user_excluded == FALSE]
          measured_events$cp_event_start_dp <- measured_events$cp_event_start_dp_2
          measured_events$cp_event_stop_dp <- measured_events$cp_event_stop_dp_2
        }

      }


      measured_events[, event_length := (cp_event_stop_dp-cp_event_start_dp)+1]
      measured_events <- measured_events[event_length >= (tmin_ms*(hz/1000))]
      end_of_prior_event_dp <- c(1, head(measured_events$cp_event_stop_dp, -1))
      start_of_next_event_dp <- c(tail(measured_events$cp_event_start_dp, -1), length(trap_trace))
      substeps <- list()
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

        if(analyzer == "covar"){
          forward_ensemble$data <- forward_ensemble$data - mean_baseline_prior
          backwards_ensemble$data <- backwards_ensemble$data - mean_baseline_after
        }

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
      ## )

      ## print(paste0("b = ", b, "id: ", measured_events$id[[event]], "; substep1: ", s1_avg, "; substep2: ", s2_avg))
    }

      running_average_f <- running_average_f/event
      running_average_b <- running_average_b/event

      forward_ensemble[, data := running_average_f]
      backwards_ensemble[, data := running_average_b]


      ensemble <-  rbind(forward_ensemble, backwards_ensemble)
      ensemble[,
               `:=`(project = options_data$project[[i]],
                    conditions = options_data$conditions[[i]],
                    date = options_data$date[[i]],
                    obs = options_data$obs[[i]],
                    bead = b)
               ]

      event_ensembles_list[[b]] <- ensemble
      substeps_list[[b]] <- data.table::rbindlist(substeps)
      ## if(analyzer == "covar"){
      ##   event_ensembles <- event_ensembles[, .(data = mean(data)), by = .(project,
      ##                                                                     conditions,
      ##                                                                     date,
      ##                                                                     obs,
      ##                                                                     direction,
      ##                                                                     forward_backward_index,
      ##                                                                     ensemble_index,
      ##                                                                     event_index)]
      ## }

      ## event_ensembles[[event]] <- ensemble

      ## ensemble_path_f <- file.path(path, "forward-ensemble-data.rds")
      ## ensemble_path_b <- file.path(path, "backwards-ensemble-data.rds")

      ## print(paste0("writing forward ", event))
      ## if(file.exists(ensemble_path_f)){
      ##   data.table::fwrite(event_ensembles[direction=="forward"], file = ensemble_path_f, sep = ",", append = TRUE)
      ## } else {
      ##   data.table::fwrite(event_ensembles[direction=="forward"], file = ensemble_path_f, sep = ",")
      ## }

      ## print(paste0("writing backwards ", event))
      ## if(file.exists(ensemble_path_b)){
      ##   data.table::fwrite(event_ensembles[direction=="backwards"], file = ensemble_path_b, sep = ",", append = TRUE)
      ## } else {
      ##   data.table::fwrite(event_ensembles[direction=="backwards"], file = ensemble_path_b, sep = ",")
      ## }

      ## event_ensembles$ms_extend_s2 <- ms_extend_s2
      ## event_ensembles$ms_extend_s1 <- ms_extend_s1
      ## event_ensembles$ms_stroke_to_skip <- ms_2_skip


      ## if(file.exists(ensemble_path)){
      ## data.table::fwrite(event_ensembles, file = ensemble_path, sep = ",", append = TRUE)
      ## } else {
      ## data.table::fwrite(event_ensembles[direction=="forward"], file = file.path(path, "forward-ensemble-data.csv"), sep = ",")
      ## data.table::fwrite(event_ensembles[direction=="backwards"], file = file.path(path, "backwards-ensemble-data.csv"), sep = ",")
      ## }
      ## saveRDS(event_ensembles[direction=="forward"], file.path(path, "forward-ensemble-data.rds"))
      ## saveRDS(event_ensembles[direction=="backwards"], file.path(path, "backwards-ensemble-data.rds"))

      ## substeps <- data.table::rbindlist(substeps)
      ## substeps_path <- file.path(path, "substeps.csv")
      ## if(file.exists(substeps_path)){
      ## data.table::fwrite(substeps, file = substeps_path, sep = ",", append = TRUE)
      ## } else {
      ## data.table::fwrite(substeps, file = substeps_path, sep = ",")
      ## }
    }


    event_ensembles_list <- data.table::rbindlist(event_ensembles_list)

    event_ensembles_list <- event_ensembles_list[, .(data = mean(data)), by = .(project,
                                                                                conditions,
                                                                                date,
                                                                                obs,
                                                                                direction,
                                                                                forward_backward_index,
                                                                                ensemble_index)]

    substeps_list <- data.table::rbindlist(substeps_list)

    ensemble_path <- file.path(path, "ensemble-data.csv")
    data.table::fwrite(event_ensembles_list, file = ensemble_path, sep = ",")

    ensemble_options <- data.table::data.table( ms_extend_s2, ms_extend_s1, ms_2_skip)
    data.table::fwrite(ensemble_options, file = file.path(path, "options-prep-ensemble.csv"), sep = ",")

    substeps_path <- file.path(path, "substeps.csv")
    data.table::fwrite(substeps_list, file = substeps_path, sep = ",")
  }
}



#' Ensemble Average
#' @param trap_selected_project full path to currently selected project
#' @param is_shiny logical. is function being used in shiny or not. 
#' @noRd
#' @import data.table
avg_ensembles <- function(project, is_shiny = TRUE){
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

    forward_backward[[i]] <- ee_data[,
      .(avg = mean(data, na.rm = TRUE),
        sd = sd(data, na.rm = TRUE),
        se = sd(data, na.rm = TRUE)/sqrt(.N)),
      by = .(conditions, direction, ensemble_index, forward_backward_index)]
}
    ####################

  ##   ee_forwards <- vector("list")
  ##   ee_backwards <- vector("list")
  ##   ee_list <- vector("list")
  ##   for(f in seq_along(ee_paths)){
  ##     print("forwards avg")

  ##     ee_con_data <- ee_fread(ee_paths[[f]], is_shiny)


  ##     ee_forwards[[f]] <- ee_con_data[direction == "forward",
  ##                                     .(data = mean(data, na.rm = TRUE)),
  ##                                       ## n = .N,
  ##                                       ## sd = sd(data, na.rm = TRUE),
  ##                                       ## se = sd(data, na.rm = TRUE)/sqrt(.N)),
  ##                                     by = .(conditions, direction, ensemble_index, forward_backward_index)]

  ##     ## ee_backwards[[f]] <- ee_con_data[direction == "backwards",
  ##     ##                                  .(data = mean(data, na.rm = TRUE)),
  ##     ##                                    ## n = .N,
  ##     ##                                    ## sd = sd(data, na.rm = TRUE),
  ##     ##                                    ## se = sd(data, na.rm = TRUE)/sqrt(.N)),
  ##     ##                                  by = .(conditions, direction, ensemble_index, forward_backward_index)]
  ##     rm(ee_con_data)

  ##   }
  ##   ## lsos()
  ##   eef <- data.table::rbindlist(ee_forwards)
  ##   rm(ee_forwards)
  ##   eef <- eef[direction == "forward",
  ##              .(avg = mean(data, na.rm = TRUE),
  ##                ## n = .N,
  ##                sd = sd(data, na.rm = TRUE),
  ##                se = sd(data, na.rm = TRUE)/sqrt(.N)),
  ##              by = .(conditions, direction, ensemble_index, forward_backward_index)]

  ##   ee_forward_data[[i]] <- eef


  ##   for(f in seq_along(backwards_ee_paths)){

  ##     print("backwards avg")

  ##     ee_con_data <- ee_fread(backwards_ee_paths[[f]], is_shiny)

  ##     ee_backwards[[f]] <- ee_con_data[direction == "backwards",
  ##                                      .(data = mean(data, na.rm = TRUE)),
  ##                                        ## n = .N,
  ##                                        ## sd = sd(data, na.rm = TRUE),
  ##                                        ## se = sd(data, na.rm = TRUE)/sqrt(.N)),
  ##                                      by = .(conditions, direction, ensemble_index, forward_backward_index)]
  ##     rm(ee_con_data)
  ##   }




  ##   eeb <- data.table::rbindlist(ee_backwards)
  ##   rm(ee_backwards)
  ##   eeb <- eeb[direction == "backwards",
  ##              .(avg = mean(data, na.rm = TRUE),
  ##                ## n = .N,
  ##                sd = sd(data, na.rm = TRUE),
  ##                se = sd(data, na.rm = TRUE)/sqrt(.N)),
  ##              by = .(conditions, direction, ensemble_index, forward_backward_index)]

  ##   ee_backwards_data[[i]] <- eeb
  ## }
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



#' Internal function to fit ensembles averages
#'
#' @param data ensemble averaged data from prep and avg ensembles
#' @param fit type of fit/
#' @param hz sampling frequency
#' @param max_l length of longest event in condition.
#' @param is_shiny logical. is used in shiny app.
#' @return nothing. Saves ensemble-data.csv in each obs-## folder.
#' @import data.table
#' @noRd
fit_ensembles <- function(data, fit, hz, max_l_forward, max_l_backwards, is_shiny = TRUE){
  print("starting fits")
## browser()

  if(is_shiny) incProgress(0.25, detail = "Fitting Forwards...")

  max_l_forward <- max_l_forward*hz
  max_l_backwards <- max_l_backwards*hz

  forward_avg <- data[direction == "forward", 
                      .(conditions,
                        ensemble_index, 
                        avg, 
                        sd, 
                        se
                        ## n
                        )]
  
  forward_nest <- forward_avg[, .(ensemble_data = list(.SD)), by = conditions]
  
  forward_nest[, `:=`(ensemble_k1_prep = lapply(ensemble_data, prep_forward_ensemble_exp, hz = hz, max_l = max_l_forward),
                      avg_tail = vapply(ensemble_data, function(x) mean(tail(x$avg, -100)), FUN.VALUE = numeric(1))
                      ## n = vapply(ensemble_data, function(x) unique(x$n),  FUN.VALUE = numeric(1)))
                      )
               ]
  
  if(fit == "2exp"){

    forward_nest[, forward_fit := lapply(ensemble_k1_prep,
                                         fit_forward_ee_2exp)
                 ]

  } else if(fit == "1exp"){
    
    forward_nest[, forward_fit := lapply(ensemble_k1_prep, 
                                         fit_forward_ee_1exp, 
                                         start = list(d1 = 5, d2 = 2, k1 = 100))
                ]
  } else if(fit == "3exp"){

    forward_nest[, forward_fit := lapply(ensemble_k1_prep,
                                         fit_forward_ee_3exp)
                 ]

  }
  
  
  forward_nest[, predict_forward := lapply(forward_fit, 
                                           predict_ee, 
                                           hz = hz)
              ]
  
  forward_nest[, `:=`(forward_fit_par_table = lapply(forward_fit, 
                                                 broom::tidy),
                      total_time_dp = sapply(ensemble_k1_prep, function(x) x[, .(length = .N)]$length )
                      )
              ] 
  
  
  if(is_shiny) incProgress(0.25, detail = "Fitting Backwards...")
  
  backwards_avg <- data[direction == "backwards", 
                      .(conditions,
                        ensemble_index, 
                        avg, 
                        sd, 
                        se
                        ## n
                        )]
  
  backwards_nest <- backwards_avg[, .(ensemble_data = list(.SD)), by = conditions]
  
  backwards_nest[, `:=`(ensemble_k2_prep = lapply(ensemble_data, prep_backwards_ensemble_exp, hz = hz, max_l = max_l_backwards),
                        backwards_baseline_shifted = lapply(ensemble_data, prep_backwards_baseline_shift, hz = hz),
                        avg_head = vapply(ensemble_data, function(x) mean(head(x$avg, 100)), FUN.VALUE = numeric(1))
                        ## n = vapply(ensemble_data, function(x) unique(x$n),  FUN.VALUE = numeric(1)))
                        )
                 ]
  
  backwards_nest[, backwards_fit := lapply(ensemble_k2_prep, 
                                           fit_backwards_ee_1exp, 
                                           start = list(d1 = 6, d2 = 2, k2 = 500))
  ]
  
  backwards_nest[, predict_backwards := lapply(backwards_fit, 
                                               predict_ee, 
                                               hz = hz,
                                               forward = FALSE)
  ]
  
  backwards_nest[, backwards_fit_par_table := lapply(backwards_fit, 
                                                     broom::tidy)
  ] 
  
  
  
  return(list(forward = forward_nest,
              backwards = backwards_nest))
  
}






##' Internal function to align mini ensemble data for ensemble averages
##' @description defines a function to align/extend all events
##' will save a .csv file for each observation in its observation folder
##'  this "mini-events-aligned.csv" file will contain all the running mean trap-data
##'  for each event extended out to the longest event per condition
##' events are extended by padding with NA values
##' @param project a character string specyfing a lasertrapr project folder
##' @return nothing. Saves ensemble-data.csv in each obs-## folder.
##' @import data.table
##' @noRd
align_mini_events <- function(project, is_shiny = FALSE){

  project_path <- file.path(path.expand("~"), "lasertrapr", project)

  #get options.csv to see what data should be included
  options_paths <-
    list.files(project_path,
               pattern = "options.csv",
               recursive = TRUE,
               full.names = TRUE)

  options_data <- data.table::rbindlist(lapply(options_paths, data.table::fread, nrows = 1), fill = TRUE)
  filtered_options <- options_data[include == TRUE & report == "success" & review == TRUE]

  filtered_options[, obs_to_avg := file.path(path.expand("~"),
                                             "lasertrapr",
                                              project,
                                              conditions,
                                              date,
                                              obs)]

  filtered_options[, measured_events_path := file.path(path.expand("~"),
                                                       "lasertrapr",
                                                       project,
                                                       conditions,
                                                       date,
                                                       obs,
                                                       "mini-measured-events.csv")]

  all_measured_events <- data.table::rbindlist(lapply(filtered_options$measured_events_path, data.table::fread), fill = TRUE)
  measured_events_filtered  <- all_measured_events

  ## calculate the length of each event
  all_lengths <- vector("list")
  for(z in 1:nrow(all_measured_events)){

    if(is_shiny) incProgress(1/(10*nrow(all_measured_events)))

    event_start <- all_measured_events$event_start[z]
    event_stop <- all_measured_events$event_stop[z]
    event_length <- length(event_start:event_stop)
    all_lengths[[z]] <- data.table(event_length = event_length,
                                 conditions = all_measured_events$conditions[z])
    }

  ## find the longest event per each condition
  all_lengths <- data.table::rbindlist(all_lengths)
  max_length_df <- all_lengths[, .(longest_event = max(event_length)), by = conditions]

  ## for every included observation align all events to the longest event per conditions
  for(o in seq_along(filtered_options$obs_to_avg)){

    if(is_shiny) incProgress(1/(10*length(filtered_options$obs_to_avg)))

    print(paste0("Analyzing ", filtered_options$obs_to_avg[o]))
    obs_path <- filtered_options$obs_to_avg[o]
## read the measured event for the observation
    measured_events <- fread(file.path(obs_path, "mini-measured-events.csv"))
    ## measured_events  <- measured_events[keep == TRUE & event_user_excluded == FALSE]
## read the trap data for the observation
    trap_data <- fread(file.path(obs_path, "trap-data.csv"))
## extend each event padding with NA
    equal_length_events <- vector("list")
    for(i in 1:nrow(measured_events)){

    if(is_shiny) incProgress(1/(10*nrow(measured_events)))

      start <- measured_events$event_start[i]
      stop <- measured_events$event_stop[i]
      event_data <- trap_data$run_mean_overlay[start:stop]
      event_data <- event_data[1:which.max(event_data)]
      ## t <- 1:length(event_data)
      ## slope <- lm(event_data~t)
      max_length <- max_length_df$longest_event[which(max_length_df$conditions == measured_events$conditions[i])]

      if(length(event_data) < max_length){
        ## extension <- predict(slope, newdata = data.frame(t = 1:max_length))
        diff_to_pad <- max_length - length(event_data)
        event_data <- c(event_data, rep(tail(event_data, 1), diff_to_pad))
        ## event_data <- c(event_data, extension[(length(event_data)+1):max_length])
      }
      ## print(length(event_data))
      equal_length_events[[i]] <- data.table(project = measured_events$project[[i]],
                                             conditions = measured_events$conditions[[i]],
                                             date = measured_events$date[[i]],
                                             obs = measured_events$obs[[i]],
                                             event = i,
                                             index = 1:length(event_data),
                                             data = event_data)
     }

    equal_length_events_df <- data.table::rbindlist(equal_length_events)
    fwrite(equal_length_events_df, file = file.path(filtered_options$obs_to_avg[o], "mini-events-aligned.csv"))
    }


  if(is_shiny) setProgress(1)

  return(invisible())
}


# defines a function to then read all the aligned data and averages it
# fitting a weighted linear regression model to the data
# this function returns the mini-ensemble-average traces and
# the linear regression fit lines for each condition and
# a ggplot with basic plot to furhter customize
# and the linear regression models are returns to get the slopes
##' Internal function to avg mini ensemble data
##' @param project a character string specyfing a lasertrapr project folder
##' @return a list with 'avg_data', 'predict_df', 'nls_mod', and 'gg'
##' @import data.table ggplot2
##' @noRd
avg_aligned_mini_events <- function(project, is_shiny = FALSE){
## browser()
 project_path <- file.path(path.expand("~"), "lasertrapr", project)

 options_paths <-
    list.files(project_path,
               pattern = "options.csv",
               recursive = TRUE,
               full.names = TRUE)

  options_data <- data.table::rbindlist(lapply(options_paths, data.table::fread, nrows = 1), fill = TRUE)
  filtered_options <- options_data[include == TRUE & report == "success" & review == TRUE]

  hz <- unique(filtered_options$hz)
  if(length(hz)>1){
   stop("Detected more than one sampling frequency (Hz). Aborting. Cannot perform ensemble averaging.")
  }

  all_conditions <- unique(filtered_options$conditions)
  all_mini_avg <- vector("list")
  all_predict_df <- vector("list")
  ## all_lm_mod <- vector("list")
  all_nls_mod <- vector("list")
  for(i in seq_along(all_conditions)){

    if(is_shiny) incProgress(1/2*length(all_conditions))

    aligned_events_paths <- list.files(file.path(project_path, all_conditions[i]),
                                 pattern = "mini-events-aligned.csv",
                                 recursive = TRUE,
                                 full.names = TRUE)

    aligned_events <- data.table::rbindlist(lapply(aligned_events_paths, data.table::fread))

    mini_avg <- aligned_events[, .(avg_nm = mean(data, na.rm = TRUE),
                                   n = sum(ifelse(is.na(data), 0, 1))), by = .(project, conditions, index)]
    mini_avg[, time_s := .I/hz]

    ## mod <- lm(avg_nm~index, data=mini_avg, weights = n)


  nls_mod <- minpack.lm::nlsLM(avg_nm ~ d1 + (1 - exp(-k1 * time_s)),
                    data = mini_avg,
                    start = list(d1 = 7, k1 = 100))

    predict_df <- data.table(x = 1:nrow(mini_avg),
                             fitted_y = predict(nls_mod),
                             conditions = all_conditions[i])

    predict_df[, time_s := .I/hz]

    all_mini_avg[[i]] <- mini_avg
    all_predict_df[[i]] <- predict_df
    all_nls_mod[[i]] <- nls_mod
  }

  if(is_shiny) setProgress(0.95)

  names(all_nls_mod) <- all_conditions

  all_mini_avg <- data.table::rbindlist(all_mini_avg)
  all_predict_df <- data.table::rbindlist(all_predict_df)

  gg <- ggplot()+
         geom_line(data = all_mini_avg,
                   aes(x = time_s,
                       y = avg_nm,
                       color = conditions),
                   alpha = 0.5)+
    ylab("Displacement (nm)")+
    xlab("Time (ms)")

gg_with_fit <- gg +
         geom_line(data = all_predict_df,
                   aes(x = time_s, y = fitted_y,
                       color = conditions),
                   linetype = "dashed")

  return(list(avg_data = all_mini_avg,
              predict_df = all_predict_df,
              nls_models = all_nls_mod,
              gg = gg,
              gg_with_fit = gg_with_fit))
}
