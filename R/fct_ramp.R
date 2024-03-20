force_ramp_analysis <- function(trap_data,
                                f = f,
                                threshold_time_ms,
                                is_shiny = FALSE){

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

  if(is.null(o$channels)) channels <- 1
  if(is.na(o$channels)) channels <- 1
  channels <- o$channels

  ## if(channels != 2) stop("Covariance requires 2 channel data")
  include <- o$include
  if(is.na(include)) include <- FALSE
  mv2nm <-  o$mv2nm
  nm2pn <- o$nm2pn
  mv2nm2 <-  o$mv2nm2
  nm2pn2 <- o$nm2pn2
  hz <- o$hz

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
        stop("Only 2 channels data can be used with force ramp")
        } else {
        not_ready <- rlang::is_empty(trap_data$processed_bead_1)
        }
        if(not_ready){
          if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
          stop('Data not processed')
          }

        if(is_shiny){
          setProgress(0.05, paste("Analyzing", conditions, obs))
          defend_if_empty(trap_data$processed_bead_1, ui = paste0(obs, ' data not processed.'), type = 'error')
        }

        if(rlang::is_empty(trap_data$stage_position)){
          showNotification(paste0(trap_data$obs, ' does not have stage data'), type = "warning")
          stop("No Stage Data")
        }



## max(trap_data$aod_position)
## min(trap_data$aod_position)

trap_data[, is_max_range := ifelse(stage_position == max(stage_position) | stage_position == min(stage_position),
                                   1,
                                   0) ]

events <- data.table::as.data.table(unclass(rle(trap_data$is_max_range)))

events[, end_event_dp := cumsum(lengths)]
events[, begin_event_dp := end_event_dp - lengths + 1 ]
events[, index := .I]

## hz <- 4000

threshold_time_dp <- threshold_time_ms/1000*hz

events_filtered <- events[lengths >= threshold_time_dp & values == 1]

          report_data  <- "success"


        opt_df <- as.data.frame(opt)

                                        # option cols to keep is a terrible name
                                        # its the columns names to keep, SO they can be removed
        options_cols_to_keep <- names(opt_df) %in% names(o)
        options_cols_to_keep <- names(opt_df)[options_cols_to_keep]

        options_df <-
          o |>
          dplyr::select(-c(options_cols_to_keep)) |>
          cbind(opt_df) |>
          dplyr::mutate( analyzer = 'ramp',
                        status = 'analyzed',
                        report = report_data,) |>
          dplyr::select(project, conditions, date, obs, everything())

        if(is_shiny == T) setProgress(0.95, detail = 'Saving Data')
        file_names <-  c('trap-data.csv',
                         'measured-events.csv',
                         #'ensemble-data.csv',
                         'hm-model-data.csv',
                         ## 'event-frequency.csv',
                         'options.csv')

        file_paths <-  file.path(path.expand("~"), "lasertrapr", project,  conditions, date, obs, file_names)

        data_to_save <- list(trap_data,
                             cp_data,
                             hm_model_results,
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
## options <- data.table::fread("~/lasertrapr/project_vinculin/vinculin/2023-07-11/obs-06/options.csv")

## trap_data$processed_bead_1 <- trap_data$raw_bead_1 * options$mv2nm

## pb1 <- RcppRoll::roll_meanl(trap_data$raw_bead_1, n = 400)

## head(trap_data)

## plot(trap_data$aod_position, type = "l")

## max(trap_data$aod_position)
## min(trap_data$aod_position)

## trap_data[, is_max_range := ifelse(aod_position == max(aod_position) | aod_position == min(aod_position), 1, 0) ]

## events <- data.table::as.data.table(unclass(rle(trap_data$is_max_range)))

## events[, end_event_dp := cumsum(lengths)]
## events[, begin_event_dp := end_event_dp - lengths + 1 ]
## events[, index := .I]

## hz <- 4000
## threshold_time_ms <- 200

## threshold_time_dp <- 1000/1000*hz

## events_filtered <- events[lengths >= threshold_time_dp & values == 1]





## all_absolute_force <- vector()
## all_relative_force <- vector()
## all_absolute_position <- vector()
## all_relative_position <- vector()
## all_last_step_time_dp <- vector()
## all_last_step_time_s <- vector()
## molecules <- vector()
## changepoint_locations <- vector("list")
## for(i in 1:nrow(events_filtered)){
##   print(i)
##   start <- events_filtered$begin_event_dp[[i]]
##   stop <- events_filtered$end_event_dp[[i]]
##   data_subset <- trap_data[start:stop]
##   pb1_sub <- na.omit(pb1[start:stop])

##   cpt_obj <- changepoint::cpt.mean(data_subset$raw_bead_1,
##                                    method = "PELT",
##                                    ## Q = 6,
##                                    ## penalty = "Manual",
##                                    ## pen.value = 400,
##                                    ## minseglen = 4000
##                                    )
##   ## cpt_obj <- changepoint::cpt.mean(pb1_sub, method = "PELT")
##   ## cpt_obj <- changepoint::cpt.mean(data_subset$raw_bead_1, method = "BinSeg", Q = 2, minseglen = 2000)
##   ## cpt_obj <- changepoint::cpt.mean(pb1_sub, method = "BinSeg", Q = 2, minseglen = 200)
##   ## plot(cpt_obj)
##   ## cpt_obj

##   if(length(changepoint::cpts(cpt_obj)) == 1){
##     ##only one molecule in binding event
##     last_step_indices <- c(1, changepoint::cpts(cpt_obj))
##     n_mol <- 1
##   } else {
##     last_step_indices <- tail(changepoint::cpts(cpt_obj), 2)
##     n_mol  <- length(changepoint::cpts(cpt_obj))
##   }

##   last_step <- data_subset[last_step_indices[[1]]:last_step_indices[[2]]]

##   time_dp <- nrow(last_step)
##   time_s <- time_dp/hz

##   absolute_position_nm <- mean(last_step$processed_bead_1)
##   event_index <- events_filtered$index[[i]]

##   baseline_after_start <- events$begin_event_dp[[(event_index+1)]]
##   baseline_mean <- mean(trap_data$processed_bead_1[baseline_after_start:(baseline_after_start+hz)])

##   relative_position_nm <- absolute_position_nm - baseline_mean

##   absolute_force_pn <- absolute_position_nm * options$nm2pn
##   relative_force_pn <- relative_position_nm * options$nm2pn

##   all_absolute_force[[i]] <- absolute_force_pn
##   all_relative_force[[i]] <- relative_force_pn
##   all_absolute_position[[i]] <- absolute_position_nm
##   all_relative_position[[i]] <- relative_position_nm
##   all_last_step_time_dp[[i]] <- time_dp
##   all_last_step_time_s[[i]] <- time_s
##   molecules[[i]] <- n_mol
##   changepoint_locations[[i]] <- changepoint::cpts(cpt_obj)+start

## }


## events_filtered[, `:=`(absolute_force_pn = all_absolute_force,
##                        relative_force_pn = all_relative_force,
##                        absolute_position_nm = all_absolute_position,
##                        relative_position_nm = all_relative_position,
##                        last_step_time_dp = all_last_step_time_dp,
##                        last_step_time_s = all_last_step_time_s,
##                        n_molecules = molecules)]




## library(dygraphs)

## add_shades <- function(x, periods, ...){
##   for(p in 1:nrow(periods)){
##     x <- dygraphs::dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], color = periods$color[[p]], ...)
##   }
##   x
## }

## add_labels_last_mol <- function(x, events, ...){
##   for(event in 1:length(events)){
##     x <- dygraphs::dyEvent(x, events$location[[event]], paste0("F", event), ...)
##   }
##   x
## }


## add_labels_cpt <- function(x, events, ...){
##   for(event in 1:length(events)){
##     x <- dygraphs::dyEvent(x, events[[event]], paste0("Change ", event), ...)
##   }
##   x
## }

## periods_df <- data.frame(start = events_filtered$begin_event_dp, stop = events_filtered$end_event_dp, color = "#ffe599")

## dygraph(data.frame(x = 1:nrow(trap_data),
##                    y = trap_data$raw_bead_1,
##                    aod = (trap_data$aod_position)/100-15))|>
##   add_shades(periods_df)|>
##   add_labels_cpt(unlist(changepoint_locations))|>
##   dyRangeSelector()
## }
