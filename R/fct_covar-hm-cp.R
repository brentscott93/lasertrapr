#' Hidden markov analysis
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.
#'
covariance_hidden_markov_changepoint_analysis <- function(trap_data,
                                               f = f,
                                               w_width = 150,
                                               w_slide = "1/2",
                                               median_w_width = 200,
                                               em_random_start,
                                               front_cp_method,
                                               back_cp_method,
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
        stop("Only 2 channels data can be used with covariance option")
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


        #### RUNNING MEAN & VAR ####
        ws <- 1
        ## if(w_slide == "1-Pt"){
        ##   ws <- 1
        ## } else if(w_slide == "1/4"){
        ##   ws <- round_any(w_width*0.25, 1)
        ## } else if(w_slide == "1/2"){
        ##   ws <- round_any(w_width*0.5, 1)
        ## } else if(w_slide == "3/4"){
        ##   ws <- round_any(w_width*0.75, 1)
        ## } else if(w_slide == "No-overlap"){
        ##   ws <- w_width
        ## }

        if(is_shiny) setProgress(0.1, detail = "Calculating Running Windows")


        ##  dat <- fread("/home/brent/sync/dani-trap/10uM-ATP/10uM-dani/2023-11-06/obs-12/231106_f2_s1_m2_dani 12.txt", skip = 68)
        ## pb1 <- dat$Trap1X[1:500000]*52
        ## pb2 <- dat$Trap2X[1:500000]*58
        ##    w_width <- 20
        ##    ws <- 1

        pb1 <- trap_data$processed_bead_1
        pb2 <- trap_data$processed_bead_2

##         covar <- zoo::rollapply(data.frame(pb1, pb2),
##                                 width = w_width,
##                                 by = ws,
##                                 FUN = \(x) cov(x[,1], x[,2]),
##                                 by.column = FALSE)
## ## covar_smooth <- covar
##            covar_smooth <- na.omit(RcppRoll::roll_medianl(covar, n = 100, by = 1))
           ## covar_smooth <- pracma::savgol(covar, 101)
 ## % Calculate the covariance.
 ##            A_filt = movmean(A, averagingWindow);
 ##            B_filt = movmean(B, averagingWindow);
 ##            AB_filt = movmean(A.*B, averagingWindow);
 ##            cov = AB_filt - A_filt.*B_filt;
        #FROM SPASM
           pb1_smooth <- na.omit(RcppRoll::roll_meanl(pb1, n = w_width, by = 1))
           pb2_smooth <- na.omit(RcppRoll::roll_meanl(pb2, n = w_width, by = 1))
           pb12_smooth <- na.omit(RcppRoll::roll_meanl(pb1*pb2, n = w_width, by = 1))
           covar <- pb12_smooth - (pb1_smooth*pb2_smooth)
## covar_smooth <- covar
           covar_smooth <- na.omit(RcppRoll::roll_medianl(covar, n = median_w_width, by = 1))
           ## covar_smooth <- pracma::savgol(covar, 101)
         ## dygraphs::dygraph(data.frame(seq_along(covar_smooth), covar_smooth))|>dygraphs::dyRangeSelector()
         ## dygraphs::dygraph(data.frame(seq_along(pb1), pb1, pb2))|>dygraphs::dyRangeSelector()
        #### HMM ####
        if(is_shiny) setProgress(0.25, detail = "HM-Model")

        hm_model_results <- fit_hm_model_to_covar_smooth(covar_smooth = covar_smooth,
                                                         em_random_start = em_random_start,
                                                         is_shiny = TRUE,
                                                         project = project,
                                                         conditions = conditions,
                                                         date = date,
                                                         obs = obs)
## ggplot(hm_model_results[1:50000])+
  ## geom_point(aes(index, covar_smooth, color = state))

        #### MEASURE EVENTS ####
        ## conversion <- ws
        if(is_shiny) setProgress(0.5, detail = "Measuring")

                                        #finds lengths of events in number of running windows
        run_length_encoding <- rle(hm_model_results$state)

                                        #converting to a tibble
        viterbi_rle <- data.table::as.data.table(do.call("cbind", run_length_encoding))

                                        #If the rle_object's last row is in state 1, get rid of that last row.
                                        #This needs to end in state 2 to capture the end of the last event
        if(tail(viterbi_rle, 1)$values == 1){
          viterbi_rle <- head(viterbi_rle, -1)
        }
                                        #Calculate the cumulative sum of the run length encoder
                                        #And splits the tibble into two seperate tables to isolate state 1 info from state 2
        viterbi_rle[, cumsum := cumsum(lengths)]

        value1 <- viterbi_rle[values == 1]
        value2 <- viterbi_rle[values == 2]

                                        #data is recmombined in a state_1 column and a state_2
                                        #the values in these columns represent the last window in either state 1 or state 2
                                        #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the estimate event duration
        hm_event_transitions <- data.table::data.table(state_1_end = value1$cumsum, state_2_end = value2$cumsum)



        #### CHANGEPOINT ####
        if(is_shiny) setProgress(0.75, detail = "Changepoint")


        cp_results <- covar_changepoint(pb_data = data.frame(pb1, pb2),
                                     hm_event_transitions = hm_event_transitions,
                                     hz = hz,
                                     w_width = w_width,
                                     median_w_width = median_w_width,
                                     value1 = value1,
                                     nm2pn = nm2pn,
                                     nm2pn2 = nm2pn2,
                                     front_cp_method = "mean/var",
                                     back_cp_method = "mean/var",
                                     look_for_cp_in_between = FALSE)

        cp_data <- cp_results$data


        cp_data$event_user_excluded <- FALSE
        cp_data$project <- project
        cp_data$conditions <- conditions
        cp_data$date <- date
        cp_data$obs <- obs
        cp_data$analyzer <- "covar"

       ####EVENT FREQ####
        if(is_shiny) setProgress(0.75, detail = "Event Frequency")

        ## event_freq <- event_frequency(processed_data,
        ##                               measured_hm_events$viterbi_rle,
        ##                               conversion,
        ##                               hz = hz,
        ##                               ends_in_state_1 = measured_hm_events$ends_in_state_1,
        ##                               project = project,
        ##                               conditions = conditions,
        ##                               date = date,
        ##                               obs = obs)

        #get some data for plotting later




        report_data  <- "success"
        ## str(cp_results$did_it_flip_vec)

        ## if(cp_results$did_it_flip_vec[[1]]){
        ##   trap_data$processed_bead_1 <- trap_data$processed_bead_1*-1
        ##   hm_model_results$covar_smooth <- hm_model_results$covar_smooth*-1
        ##   }
        ## if(cp_results$did_it_flip_vec[[2]]){
        ##   trap_data$processed_bead_2 <- trap_data$processed_bead_2*-1
        ##   }


        opt_df <- as.data.frame(opt)

                                        # option cols to keep is a terrible name
                                        # its the columns names to keep, SO they can be removed
        options_cols_to_keep <- names(opt_df) %in% names(o)
        options_cols_to_keep <- names(opt_df)[options_cols_to_keep]

        options_df <-
          o |>
          dplyr::select(-c(options_cols_to_keep)) |>
          cbind(opt_df) |>
          dplyr::mutate( analyzer = 'covar',
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
