

#' Hidden markov analysis
#' @noRd
#' @param trap_data A dataframe of all 'trap-data' files.
#' @param f The 'f' reactiveValues from app.
#' @param em_random_start A logical indicating if the EM-Algorithm should randomly start fitting gaussians.
#'
isometric_force_clamp_analysis <- function(trap_data,
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

        not_ready <- is_empty(trap_data$processed_bead)
        if(not_ready){
          if(is_shiny) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
          stop('Data not processed')
          }

        if(is_shiny){
          setProgress(0.05, paste("Analyzing", conditions, obs))
          defend_if_empty(trap_data$processed_bead, ui = paste0(obs, ' data not processed.'), type = 'error')
        }

        #####################3333
        hz <- 20000/1000
        transducer_bead <- rep(
                            c(
                              rnorm(200*hz, 0, 8),
                              rnorm((15*hz), 0, 4),
                              rnorm(100*hz, 0, 8),
                              rnorm((25*hz), 0, 4),
                              rnorm(50*hz, 0, 8),
                              rnorm((50*hz), 0, 4),
                              rnorm(500*hz, 0, 8),
                              rnorm((10*hz), 0, 4),
                              rnorm(150*hz, 0, 8),
                              rnorm((20*hz), 0, 4),
                              rnorm(100*hz, 0, 8)),
                              10)

        motor_bead <- rep(
                            c(
                              rnorm(200*hz, 50, 10),
                              rnorm((15*hz), 100, 2),
                              rnorm(100*hz, 50, 10),
                              rnorm((25*hz), 200, 3),
                              rnorm(50*hz, 50, 10),
                              rnorm((50*hz), 250, 2),
                              rnorm(500*hz, 50, 10),
                              rnorm((10*hz), 120, 2),
                              rnorm(150*hz, 50, 10),
                              rnorm((20*hz), 150, 4),
                              rnorm(100*hz, 50, 10)),
                              10)




        force_clamp_data <- data.table(time = 1:length(motor_bead)/20000,
                                       transducer_bead = transducer_bead,
                                       motor_bead = motor_bead)


        ## dygraphs::dygraph(force_clamp_data)
        run_mean_motor <- na.omit(RcppRoll::roll_meanl(force_clamp_data$motor_bead, n = 20, by = 10))
        run_var_motor <- na.omit(RcppRoll::roll_varl(force_clamp_data$motor_bead, n = 20, by = 10))

        run_mean_transducer <- na.omit(RcppRoll::roll_meanl(force_clamp_data$transducer_bead, n = 20, by = 10))
        run_var_transducer <- na.omit(RcppRoll::roll_varl(force_clamp_data$transducer_bead, n = 20, by = 10))


        hm_model_results <- fit_hm_model(
                                         ## trap_data = trap_data,
                                         run_mean = run_mean_motor,
                                         run_var = run_var_transducer,
                                         ## use_channels = use_channels,
                                         em_random_start = em_random_start,
                                         is_shiny = F,
                                         project = project,
                                         conditions = conditions,
                                         date = date,
                                         obs = obs)

  seed <- floor(runif(1, 0, 1e6))

    hmm <- depmixS4::depmix(list(run_var_transducer~1,
                                 run_mean_motor~1),
                            data = data.frame(run_mean_motor = run_mean_motor,
                                              run_var_transducer = run_var_transducer),
                            nstates = 2,
                            family = list(stats::gaussian(),
                                          stats::gaussian()))


  hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                              0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                              0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess


  sd_run_mean <- sd(run_mean_motor)
  mean_run_var <- mean(run_var_transducer)
  sd_run_var <- sd(run_var_transducer)

  if(em_random_start == T){
    hmm_fit <- depmixS4::fit(hmm, emcontrol = depmixS4::em.control(random.start = TRUE))
  } else {
    estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, 0, sd_run_mean,
                                mean_run_var/2, sd_run_var, 3, sd_run_mean*2)
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
    obs_trap_data_exit <- trap_data  %>%
      dplyr::mutate(report = 'hmm-error',
                    analyzer = 'hm/cp')
    data.table::fwrite(obs_trap_data_exit, file = file.path(path.expand('~'), 'lasertrapr', project, conditions, date, obs, 'trap-data.csv'), sep = ",")
    if(is_shiny) showNotification('Skipping...HM-Model starts in state 2. Try trimming beginnging of obs.', type = 'warning')
    stop("HM-Model Starts in State 2. Try trimming the beginning of the obs.")
  }


  sum_fit <- depmixS4::summary(hmm_fit)
  base_var <- sum_fit[[1]]
  event_var <- sum_fit[[2]]

  s2n <- base_var/event_var

  #save running mean, var, & state for dygraph
  data <- data.table(run_mean_motor = unname(run_mean_motor),
                         run_var_transducer = unname(run_var_transducer),
                         state = hmm_posterior$state,
                         index = 1:length(hmm_posterior$state),
                         var_signal_ratio = s2n)


        ggplot(data, aes(x = index))+
          geom_point(aes(y = run_mean_motor, color = as.factor(state)))

        ggplot(data, aes(x = index))+
          geom_point(aes(y = run_var_transducer, color = as.factor(state)), size = 0.2)


        ggplot(data, aes(x = index))+
          geom_line(aes(y = run_mean_motor))+
          geom_line(aes(y = run_var_transducer), color = "red")
