#' Prep Ensembles for Ensemble Average
#'
#' @param trap_selected_project 
#' @param ms_extend_s2 
#' @param ms_extend_s1 
#' @param hz 
#'
#' @return nothing. Saves ensemble-data.csv in each obs-## folder. 
#' @noRd
prep_ensemble <- function(trap_selected_project,
                          ms_extend_s2 = 3, 
                          ms_extend_s1 = 3, 
                          ms_2_skip,
                          hz,
                          is_shiny = TRUE){
  # trap_selected_project <- "~/lasertrapr/project_myoV-phosphate"
  # ms_extend_s2 = 3
  # ms_extend_s1 = 3
  if(is_shiny) incProgress(0.01, detail = "Removing Old Files")
  
  old_files <- 
    lasertrapr:::list_files(
      trap_selected_project,
      pattern = "ensemble-data.csv",
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
  
  options_data %<>%
    dplyr::filter(
      report == "success",
      review == T,
      include == T)
  
  event_file_paths <- 
    options_data %>% 
      unite('path', c(project, conditions, date, obs), sep = "/") %>% 
      pull(path) %>% 
      paste0("~/lasertrapr/", ., '/measured-events.csv')
  
  event_files_filtered <- 
    data.table::rbindlist(
      lapply(
        event_file_paths, 
        data.table::fread
        )
    )
 
  longest_event_df <- 
    event_files_filtered %>%
    dplyr::filter(keep == TRUE & event_user_excluded == FALSE) %>% 
    dplyr::group_by(conditions) %>% 
    dplyr::summarize(longest_event = max(cp_time_on_dp))
  
  dp_extend_s2 <- ms_extend_s2*hz/1000
  dp_extend_s1 <- ms_extend_s1*hz/1000
  
  for(i in seq_len(nrow(options_data))){
    print(paste0("i=", i))
    if(is_shiny) incProgress(1/nrow(options_data), detail = paste0(i, " of ", nrow(options_data)))
    path <- file.path(path.expand("~"), 
                      "lasertrapr",
                      options_data$project[[i]],
                      options_data$conditions[[i]],
                      options_data$date[[i]],
                      options_data$obs[[i]])
    trap_data_path <- file.path(path, "trap-data.csv")
    trap_trace <- data.table::fread(trap_data_path, select = "processed_bead")$processed_bead
    measured_events_path <- file.path(path, "measured-events.csv")
    measured_events <-  data.table::fread(measured_events_path, 
                                          select = c("conditions",
                                                     "cp_event_start_dp",
                                                     "cp_event_stop_dp",
                                                     "keep",
                                                     "front_signal_ratio",
                                                     "back_signal_ratio",
                                                     "is_positive",
                                                     "event_user_excluded"))
    
    measured_events %<>% dplyr::filter(keep == TRUE & event_user_excluded == FALSE)
    
    event_ensembles <- list()
    for(event in seq_len(nrow(measured_events))){
      #print(paste0("event=", event))
      start <- measured_events$cp_event_start_dp[[event]]
      stop <- measured_events$cp_event_stop_dp[[event]]
      event_chunk <- trap_trace[start:stop]
      longest_event <- longest_event_df[which(longest_event_df$conditions == measured_events$conditions[[event]]),]
      longest_event <- longest_event$longest_event
      if(length(event_chunk) == longest_event){
        forward_event <- data.frame(data = event_chunk,
                                    ensemble_index = 0:(longest_event-1))
        backwards_event <- data.frame(data = event_chunk,
                                      ensemble_index = -(longest_event-1):0)
      } else {
        s2_avg <- mean(trap_trace[(stop - dp_extend_s2):stop])
        time_diff <- longest_event - length(event_chunk)
        forward_extended_event <- c(event_chunk, rep(s2_avg, time_diff))
        forward_event <- data.frame(data = forward_extended_event,
                                    ensemble_index = 0:(longest_event-1))
        delay_s1_start <- ms_2_skip*hz/1000
        s1_total_time <- delay_s1_start+dp_extend_s1
        if(length(event_chunk) <= s1_total_time) next
        s1_avg <-  mean(event_chunk[(delay_s1_start:(delay_s1_start+dp_extend_s1))])
        backwards_extended_event <- c(rep(s1_avg, (time_diff+delay_s1_start)), event_chunk[-c(1:delay_s1_start)])
        backwards_event <- data.frame(data = backwards_extended_event,
                                      ensemble_index = -(longest_event-1):0)
      }
      
      ms_10 <- 10*hz/1000
      end_forward_base <- start - 1
      before_forwards <- data.frame(data = trap_trace[(end_forward_base - ((ms_10*5)-1)):end_forward_base],
                                    ensemble_index = -(ms_10*5):-1)
      start_backwards_base <- stop + 1
      after_backwards <- data.frame(data = trap_trace[start_backwards_base:(start_backwards_base + ((ms_10*5)-1))],
                                    ensemble_index = 1:(ms_10*5))
      
      forward_ensemble <- 
        rbind(before_forwards, forward_event) %>% 
        mutate(direction = "forward",
               forward_backward_index = -(ms_10*5):(longest_event-1),
               event_index = event,
               signal_ratio =  measured_events$front_signal_ratio[[event]])
      
      backwards_ensemble <- 
        rbind(backwards_event, after_backwards) %>% 
        mutate(direction = "backwards",
               forward_backward_index = longest_event:((2*longest_event)+((ms_10*5)-1)),
               event_index = event,
               signal_ratio = measured_events$back_signal_ratio[[event]])
      
      ensemble <- 
        rbind(forward_ensemble, backwards_ensemble) %>% 
        mutate(project = options_data$project[[i]],
               conditions = options_data$conditions[[i]],
               date = options_data$date[[i]],
               obs = options_data$obs[[i]]) %>% 
        arrange(forward_backward_index)
      
      event_ensembles[[event]] <- ensemble
    }
    ensemble_data <- data.table::rbindlist(event_ensembles)
    ensemble_data$ms_extend_s2 <- ms_extend_s2
    ensemble_data$ms_extend_s1 <- ms_extend_s1
    ensemble_data$ms_stroke_to_skip <- ms_2_skip
    ensemble_path <- file.path(path, "ensemble-data.csv")
    data.table::fwrite(ensemble_data, file = ensemble_path, sep = ",")
  }
}



#' Ensemble Average
#' @param trap_selected_project full path to currently selected project
#' @param is_shiny logical. is function being used in shiny or not. 
#' @noRd
avg_ensembles <- function(project, is_shiny = TRUE){
  #con <-lasertrapr:::list_dir(path = "~/lasertrapr/project_hitch-simulations")
  #project <- "project_hitch-simulations"
  project_path <- file.path("~", "lasertrapr", project)
  con <- list_dir(path = project_path)
  con <- con[grep("summary", con$name, invert = TRUE, ignore.case = TRUE),]$name
  
  ee_forward_data <- list()
  ee_backwards_data <- list()
  for(i in 1:length(con)){
    if(is_shiny) incProgress(1/(length(con)*2), detail = paste0("Reading ensembles from ", con[[i]]))
     
    ee_paths <- list.files(path = file.path(project_path, con[[i]]),
                           recursive = TRUE,
                           full.names = TRUE, 
                           pattern = "ensemble-data.csv")
    
    ee_con_data <- data.table::rbindlist(lapply(ee_paths , ee_fread, is_shiny=is_shiny)) 
      
    
    ee_forward_data[[i]] <- ee_con_data[direction == "forward",  
                                        .(avg = mean(data, na.rm = TRUE),
                                          n = .N,
                                          sd = sd(data, na.rm = TRUE),
                                          se = plotrix::std.error(data, na.rm = TRUE)), 
                                        by = .(conditions, direction, ensemble_index, forward_backward_index)]
    
    ee_backwards_data[[i]] <- ee_con_data[direction == "backwards",  
                                          .(avg = mean(data, na.rm = TRUE),
                                            n = .N,
                                            sd = sd(data, na.rm = TRUE),
                                            se = plotrix::std.error(data, na.rm = TRUE)), 
                                          by = .(conditions, direction, ensemble_index, forward_backward_index)]
    rm(ee_con_data)
    
  }
  if(is_shiny) incProgress(1/(length(con)*2), detail = paste0("Combining all data", con[[i]]))
  ee_backwards_data <- data.table::rbindlist(ee_backwards_data)
  ee_forward_data <- data.table::rbindlist(ee_forward_data)
  forward_backward <- rbind(ee_forward_data, ee_backwards_data)

   return(forward_backward)
  #if(is_shiny) incProgress(1/(length(con)*2), message = paste0("Plotting...", con[[i]]))
  # ggplot(data = forward_backward)+
  #   geom_point(aes(x = forward_backward_index, 
  #                  y = avg, 
  #                  color = conditions), 
  #              alpha = 0.3,
  #              shape = 16)+
  #   facet_wrap(~conditions, scales = "free_x")+
  #   xlab("Displacement (nm)")+
  #   
  #   theme_cowplot()+
  #   theme(
  #     strip.background = element_rect(fill = "transparent"),
  #     legend.position = "none"
  #   )
  
  #ee_forward_data <- data.table::rbindlist(ee_forward_data)
  # 
  # forward_avg <-
  #   ee_forward_data %>% 
  #   as_tibble() %>% 
  #   dplyr::select(conditions, ensemble_index, avg, sd, se, n) %>% 
  #   group_by(conditions) %>% 
  #   tidyr::nest(ensemble = c(ensemble_index, avg, sd, se, n))
  # 
  #ee_backwards_data <- data.table::rbindlist(ee_backwards_data)
  # 
  # backwards_avg <- 
  #   ee_backwards_data %>% 
  #   as_tibble() %>% 
  #   dplyr::select(conditions, ensemble_index, avg, sd, se, n) %>% 
  #   group_by(conditions) %>% 
  #   tidyr::nest(ensemble = c(ensemble_index, avg, sd, se, n))
  
  
}

#data <- forward_backward
fit_ensembles <- function(data, fit, start_list, hz){
  #browser()
  forward_avg <- data[direction == "forward", 
                      .(conditions,
                        ensemble_index, 
                        avg, 
                        sd, 
                        se, 
                        n)]

  forward_nest <- forward_avg[, .(ensemble_data = list(.SD)), by = conditions]
  
  forward_nest[, `:=`(ensemble_k1_prep = lapply(ensemble_data, prep_forward_ensemble_exp, hz = hz),
                      avg_tail = vapply(ensemble_data, function(x) mean(tail(x$avg, -100)), FUN.VALUE = numeric(1)),
                      n = vapply(ensemble_data, function(x) unique(x$n),  FUN.VALUE = numeric(1)))]
  
  forward_nest[, forward_fit_2exp := lapply(ensemble_k1_prep, fit_forward_ee_2exp, start = list(d1 = 5, d2 = 2, k0 = 1000, k1 = 100))]
  
  forward_nest[, predict_forward_2exp := lapply(forward_fit_2exp, predict_ee, hz = hz)] 
  
  forward_nest[, forward_2exp_par_table := lapply(forward_fit_2exp, broom::tidy)] 
  
  return(forward_avg)
  
}
