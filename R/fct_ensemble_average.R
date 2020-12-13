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
                          hz){
  trap_data_paths <- 
    lasertrapr:::list_files(
      trap_selected_project,
      pattern = "trap-data.csv",
      recursive = TRUE)
  
  trap_data <- 
    data.table::rbindlist(
      lapply(
        trap_data_paths$path, 
        data.table::fread, 
        nrows = 1), 
      fill = TRUE)
  
  trap_data %<>%
    dplyr::filter(
      report == "success",
      review == T,
      include == T)
  
  event_file_paths <- 
    trap_data %>% 
    unite('path', c(project, conditions, date, obs), sep = "/") %>% 
    pull(path) %>% 
    paste0("~/lasertrapr/", ., '/measured-events.csv')
  
  event_files_filtered <- 
    data.table::rbindlist(
      lapply(
        event_file_paths, 
        data.table::fread)
    )
  
  longest_event_df <- 
    event_files_filtered %>%
    dplyr::filter(keep == TRUE) %>% 
    dplyr::group_by(conditions) %>% 
    dplyr::summarize(longest_event = max(cp_time_on_dp))
  
  dp_extend_s2 <- ms_extend_s2*hz/1000
  dp_extend_s1 <- ms_extend_s1*hz/1000
  
  for(i in seq_len(nrow(trap_data))){
    incProgress(1/nrow(trap_data), detail = paste0(i, " of ", nrow(trap_data)))
    path <- file.path(path.expand("~"), 
                      "lasertrapr",
                      trap_data$project[[i]],
                      trap_data$conditions[[i]],
                      trap_data$date[[i]],
                      trap_data$obs[[i]])
    trap_data_path <- file.path(path, "trap-data.csv")
    trap_trace <- data.table::fread(trap_data_path, select = "processed_bead")$processed_bead
    measured_events_path <- file.path(path, "measured-events.csv")
    measured_events <-  data.table::fread(measured_events_path, 
                                          select = c("conditions",
                                                     "cp_event_start_dp",
                                                     "cp_event_stop_dp",
                                                     "keep",
                                                     "front_signal_ratio",
                                                     "back_signal_ratio"))
    measured_events %<>% filter(keep == TRUE)
    event_ensembles <- list()
    for(event in seq_len(nrow(measured_events))){
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
        s1_start <- start + (hz/1000)
        s1_avg <-  mean(trap_trace[(s1_start:(s1_start+dp_extend_s1))])
        backwards_extended_event <- c(rep(s1_avg, (time_diff+s1_start)), event_chunk[c(1:s1_start)])
        backwards_event <- data.frame(data = backwards_extended_event,
                                      ensemble_index = -(longest_event-1):0)
      }
      
      ms_10 <- 10*hz/1000
      end_forward_base <- start - 1
      before_forwards <- data.frame(data = trap_trace[((end_forward_base - ms_10)+1):end_forward_base],
                                    ensemble_index = -ms_10:-1)
      start_backwards_base <- stop + 1
      after_backwards <- data.frame(data = trap_trace[start_backwards_base:(start_backwards_base + (ms_10-1))],
                                    ensemble_index = 1:ms_10)
      
      forward_ensemble <- 
        rbind(before_forwards, forward_event) %>% 
        mutate(direction = "forward",
               forward_backward_index = -ms_10:(longest_event-1),
               event_index = event,
               signal_ratio =  measured_events$front_signal_ratio[[event]])
      
      backwards_ensemble <- 
        rbind(backwards_event, after_backwards) %>% 
        mutate(direction = "backwards",
               forward_backward_index = longest_event:((2*longest_event)+(ms_10-1)),
               event_index = event,
               signal_ratio = measured_events$back_signal_ratio[[event]])
      
      ensemble <- 
        rbind(forward_ensemble, backwards_ensemble) %>% 
        mutate(project = trap_data$project[[i]],
               conditions = trap_data$conditions[[i]],
               date = trap_data$date[[i]],
               obs = trap_data$obs[[i]]) %>% 
        arrange(forward_backward_index)
      
      event_ensembles[[event]] <- ensemble
    }
    ensemble_data <- data.table::rbindlist(event_ensembles)
    ensemble_path <- file.path(path, "ensemble-data.csv")
    data.table::fwrite(ensemble_data, file = ensemble_path, sep = ",")
  }
}
