#' Analysis for isometric force clamp data
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.
#'
isometric_force_clamp_analysis <- function(trap_data,
                                           f = f,
                                           w_width = 20,
                                           w_slide = "1/2",
                                           em_random_start,
                                           is_shiny = F,
                                           ...){
  ## browser()
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
  ## mv2nm <-  o$mv2nm
  ## nm2pn <- o$nm2pn
  hz <- o$hz
  feedback_motor_bead <- o$feedback_motor_bead


  ## project = "project_feedback-test"
  ## conditions = "unregulated"
  ## date = "2023-06-02"
  ## obs = "obs-01"

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

        ## not_ready <- rlang::is_empty(trap_data$processed_bead)
        ## if(not_ready){
        ##   if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
        ##   stop('Data not processed')
        ##   }

        ## if(is_shiny){
        ##   setProgress(0.05, paste("Analyzing", conditions, obs))
        ##   defend_if_empty(trap_data$processed_bead, ui = paste0(obs, ' data not processed.'), type = 'error')
        ## }

        #####################3333
        ## library(data.table)
        ## hz <- 20000/1000
        ## transducer_bead <- rep(
        ##                     c(
        ##                       rnorm(200*hz, 0, 8),
        ##                       rnorm((15*hz), 0, 4),
        ##                       rnorm(100*hz, 0, 8),
        ##                       rnorm((25*hz), 0, 4),
        ##                       rnorm(50*hz, 0, 8),
        ##                       rnorm((50*hz), 0, 4),
        ##                       rnorm(500*hz, 0, 8),
        ##                       rnorm((10*hz), 0, 4),
        ##                       rnorm(150*hz, 0, 8),
        ##                       rnorm((20*hz), 0, 4),
        ##                       rnorm(100*hz, 0, 8)),
        ##                       10)

        ## motor_bead <- rep(
        ##                     c(
        ##                       rnorm(200*hz, 50, 10),
        ##                       rnorm((15*hz), 100, 2),
        ##                       rnorm(100*hz, 50, 10),
        ##                       rnorm((25*hz), 200, 3),
        ##                       rnorm(50*hz, 50, 10),
        ##                       rnorm((50*hz), 250, 2),
        ##                       rnorm(500*hz, 50, 10),
        ##                       rnorm((10*hz), 120, 2),
        ##                       rnorm(150*hz, 50, 10),
        ##                       rnorm((20*hz), 150, 4),
        ##                       rnorm(100*hz, 50, 10)),
        ##                       10)

        if(o$feedback_motor_bead == 1){
          motor_bead <- trap_data$processed_bead_1
          transducer_bead <- trap_data$processed_bead_2
        } else {
          motor_bead <- trap_data$processed_bead_2
          transducer_bead <- trap_data$processed_bead_1
        }

        force_clamp_data <- data.table(time = 1:length(motor_bead)/hz,
                                       transducer_bead = transducer_bead,
                                       motor_bead = motor_bead,
                                       index = 1:length(motor_bead))

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

        # calculate running window transformations for event ID by Hidden Markov Model
        run_mean_motor <- na.omit(RcppRoll::roll_meanl(force_clamp_data$motor_bead, n = w_width, by = ws))
        ## run_var_motor <- na.omit(RcppRoll::roll_varl(force_clamp_data$motor_bead, n = 20, by = 10))

        ## run_mean_transducer <- na.omit(RcppRoll::roll_meanl(force_clamp_data$transducer_bead, n = 20, by = 10))
        run_var_transducer <- na.omit(RcppRoll::roll_varl(force_clamp_data$transducer_bead, n = w_width, by = ws))

        seed <- floor(runif(1, 0, 1e6))

        hmm <- depmixS4::depmix(run_mean_motor~1,
                                data = data.frame(run_mean_motor = run_mean_motor),
                                nstates = 2,
                                family = stats::gaussian())


        hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                                    0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                                    0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess


        if(em_random_start == T){
          hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = TRUE))
        } else {
          estimate_hmm_gaussians <- c(mean(run_mean_motor), sd(run_mean_motor),
                                      max(run_mean_motor), sd(run_mean_motor)*2)

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
            hmm_posterior <- depmixS4::posterior(hmm_fit)
            hmm_repeat <- hmm_repeat + 1
          }
        }

                                        #purposesefully skip data if hm-model starts in state 2. All calculations assume it starts in state 1
        if(hmm_posterior$state[[1]] == 2){
    # writeLines(c("Skipping",
    #              trap_data$obs,
    #              "HMM starts in State 2. Try trimming the beginning of the observation."), error_file);
          obs_o_exit <- o  |>
            dplyr::mutate(report = 'hmm-error',
                          analyzer = 'hm/cp')
          data.table::fwrite(obs_trap_data_exit, file = file.path(path.expand('~'), 'lasertrapr', project, conditions, date, obs, 'options'), sep = ",")
          if(is_shiny) showNotification('Skipping...HM-Model starts in state 2. Try trimming beginnging of obs.', type = 'warning')
          stop("HM-Model Starts in State 2. Try trimming the beginning of the obs.")
        }

      sum_fit <- depmixS4::summary(hmm_fit)
      base_var <- sum_fit[[1]]
      event_var <- sum_fit[[2]]

      s2n <- base_var/event_var

     #save running mean, var, & state for dygraph
      hm_model_results <- data.table(run_mean_motor = unname(run_mean_motor),
                                     run_var_transducer = unname(run_var_transducer),
                                     state = hmm_posterior$state,
                                     index = 1:length(hmm_posterior$state))

        ## conversion <- ws
        if(is_shiny) setProgress(0.5, detail = "Measuring")
        run_length_encoding <- rle(hm_model_results$state)

                                        #converting to a tibble
        viterbi_rle <- data.table::as.data.table(do.call("cbind", run_length_encoding))

  #Calculate the cumulative sum of the run length encoder
        viterbi_rle[, cumsum_windows := cumsum(lengths)]

  #If the rle_object's last row is in state 1, get rid of that last row.
  #This needs to end in state 2 to capture the end of the last event
        if(tail(viterbi_rle, 1)$values == 1){
          viterbi_rle <- viterbi_rle[-length(viterbi_rle$values)]
        }

   #split data into state 1 and state 2, to recombine into a data.table
  # where 1 column is the window where state 1 ends and the other is window where state 2 ends
        split_data <- split(viterbi_rle, viterbi_rle$values)

        hm_event_transitions <- data.table::data.table(state_1_end = split_data[[1]]$cumsum_windows,
                                                 state_2_end = split_data[[2]]$cumsum_windows )

        time_prior <- viterbi_rle[values == 1]
        hm_time_ons <- viterbi_rle[values == 2]

                                        # loop over rows of hm_event_transitions
                                        # applying changepoint to beginning/end of event subsets
        cp_starts <- vector()
        cp_stops <- vector()
        keep_event <- vector()
        relative_displacements <- vector()
        absolute_displacements <- vector()
        baseline_position_prior <- vector()
        attachment_durations <- vector()
        displacement_markers <- vector()
        cp_found_start <- vector()
        cp_found_stop <- vector()

        for(i in 1:nrow(hm_event_transitions)){
          print(i)
                                        #estimated window index that the event starts and ends at from window time
          estimated_start <- (hm_event_transitions$state_1_end[[i]] + 1 )
          estimated_stop <-  hm_event_transitions$state_2_end[[i]]
                                        #
          forward_cp_window_stop <- ceiling((estimated_start * ws) + ws)
          backwards_cp_window_start <- floor((estimated_stop * ws) - ws)
                                        #
          forward_cp_window_start <- floor((estimated_start - 1) * ws)
          backwards_cp_window_stop <- ceiling((estimated_stop + 1)) * ws
                                        #
          forward_event_chunk <- force_clamp_data[forward_cp_window_start:forward_cp_window_stop,]
                                        #
          ## ggplot(forward_event_chunk,
          ##        aes(x = 1:nrow(forward_event_chunk)))+
          ##   geom_line(aes(y = motor_bead), color = "red")+
          ##   geom_line(aes(y = transducer_bead))
                                        #
          if(backwards_cp_window_stop >= nrow(force_clamp_data)) backwards_cp_window_stop <- nrow(force_clamp_data)
                                        #
          backwards_event_chunk <- force_clamp_data[backwards_cp_window_start:backwards_cp_window_stop,]
                                        #
          ## ggplot(backwards_event_chunk,
          ##        aes(x = 1:nrow(backwards_event_chunk)))+
          ##   geom_line(aes(y = motor_bead), color = "red")+
          ##   geom_line(aes(y = transducer_bead))
                                        #
                                        #if data has NA skip - helped reduce errors
          has_na <- table(is.na(forward_event_chunk$data))
          has_na2 <- table(is.na(backwards_event_chunk$data))
          if(length(has_na) > 1){
            keep_event[[i]] <- FALSE
            next
          } else {
            forward_cpt_object <- changepoint::cpt.mean(na.omit(forward_event_chunk$motor_bead),
                                                        penalty = "MBIC",
                                                        method = 'AMOC')
          }
                                        #
          if(length(has_na2) > 1){
                                        # better_time_on_stops[[i]] <- NA
                                        # cp_found_stop[[i]] <- FALSE
            keep_event[[i]] <- FALSE
          } else {
            backwards_cpt_object <- changepoint::cpt.mean(na.omit(backwards_event_chunk$motor_bead),
                                                          penalty = "MBIC",
                                                          method = 'AMOC')
          }
                                        # plot(forward_cpt_object)
                                        # plot(backwards_cpt_object)
          event_on <- try(changepoint::cpts(forward_cpt_object))
          event_off <- try(changepoint::cpts(backwards_cpt_object))
                                        #if no changepoint id'd skip
          if(identical(event_on, numeric(0))  || class(event_on) == 'try-error'){
            cp_starts[[i]] <- NA
            cp_found_start[[i]] <- FALSE
          } else {
                                        #or record change point index
            cp_start <- forward_event_chunk$index[event_on]
            cp_starts[[i]] <- cp_start
            cp_found_start[[i]] <- TRUE
          }
                                        #do same for backwards
          if(identical(event_off, numeric(0))  || class(event_off) == 'try-error'){
            cp_stops[[i]] <- NA
            cp_found_stop[[i]] <- FALSE
          } else {
                                        #or record the index where this occurred in the previous attempt
            cp_stop <- backwards_event_chunk$index[event_off] - 1
            cp_stops[[i]] <- cp_stop
            cp_found_stop[[i]] <- TRUE
          }
                                        #
          if(identical(event_on, numeric(0)) ||
             identical(event_off, numeric(0)) ||
             class(event_off) == 'try-error' ||
             class(event_on) == 'try-error'){
            keep_event[[i]] <- FALSE
            next
          }
                                        #find length of event
          attachment_durations[[i]] <- nrow(force_clamp_data[cp_start:cp_stop,])
          length_of_event <- nrow(force_clamp_data[cp_start:cp_stop,])
                                        #
                                        #
          if(length_of_event <= 0 ||  cp_found_start[[i]] == FALSE || cp_found_stop[[i]] == FALSE || cp_start >= cp_stop){
            keep <- FALSE
            keep_event[[i]] <- FALSE
            absolute_displacements[[i]] <- NA
            relative_displacements[[i]] <- NA
            baseline_position_prior[[i]] <- NA
            next
          } else {
            keep <- TRUE
            keep_event[[i]] <- TRUE
          }
                                        #
                                        # calculate the average position of the baseline preceding event
          mb <- force_clamp_data$motor_bead
          if(i == 1){
            baseline_position <- mean(mb[1]:mb[cp_start-1])
          } else {
            prior_event_end <- cp_stops[[i-1]]+1
            baseline_position <- mean(mb[prior_event_end:(cp_start-1)])
          }
                                        #
          baseline_position_prior[[i]] <- baseline_position
          absolute_displacements[[i]] <- mean(mb[cp_start:cp_stop])
          relative_displacements[[i]] <- mean(mb[cp_start:cp_stop]) - baseline_position
          displacement_markers[[i]] <- round(mean(c(cp_start, cp_stop)), 0)
                                        #
  } #loop close

        if(feedback_motor_bead == 1){
          nm2pn <- o$nm2pn
        } else {
          nm2pn <- o$nm2pn2
        }

        analyzed_force_clamp <-
          data.table(
            project = project,
            conditions = conditions,
            date = date,
            obs = obs,
            id = 1:nrow(hm_event_transitions),
            cp_found_start = cp_found_start,
            cp_found_stop = cp_found_stop,
            cp_event_start_dp = cp_starts,
            cp_event_stop_dp = cp_stops,
            midpoint_index_dp = displacement_markers,
            time_on_dp = attachment_durations,
            time_on_s = attachment_durations/hz,
            time_on_ms = attachment_durations/hz*1000,
            baseline_position_prior_nm = baseline_position_prior,
            absolute_displacements_nm = absolute_displacements,
            displacement_nm = relative_displacements,
            force_pn = relative_displacements*nm2pn,
            event_user_excluded = FALSE,
            keep = keep_event
          )




        o$analyzer <- "ifc"
        o$status <- "analyzed"
        o$report <- "success"
        o$w_slide <- w_slide
        o$w_width <- w_width

        if(is_shiny == T) setProgress(0.95, detail = 'Saving Data')

        file_names <-  c(
          ## 'trap-data.csv',
          'ifc-measured-events.csv',
          'hm-model-data.csv',
          'options.csv')

        file_paths <-  file.path(path.expand("~"), "lasertrapr", project,  conditions, date, obs, file_names)

        data_to_save <- list(
          ## trap_data,
          analyzed_force_clamp,
          hm_model_results,
          o)

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
