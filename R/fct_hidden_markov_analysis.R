# 
# hidden_markov_analysis <- function(trap_data, f, hz = 5000, em_random_start, is_shiny = F){
#   report_data  <- "error"
#  # withProgress(message = 'Analyzing trap data', value = 0, max = 1, min = 0, {
# 
#      #trap_data <- trap
#     #rds_file_path <- list_files('~/lasertrapr/project_new/new/2020-07-13/obs-01', pattern = 'trap-data.rds', recursive = T)
#     #trap_data %<>% mutate(rds_file_path = rds_file_path$path )
#     #em_random_start <- FALSE
#     # trap_data %<>% dplyr::filter(include == T)
# 
#     error_file <- file(file.path(f$date$path, "error-log.txt"), open = "a")
#     #loop will start 
#     #for(folder in seq_along(trap_data$obs)){
#       tryCatch({
#         not_ready <- is_empty(trap_data$processed)
#         if(not_ready == T){
#           if(is_shiny == T) showNotification(paste0(trap_data$obs, ' not processed. Skipping...'), type = 'warning')
#           next
#           }
#                   
#         report_data <- "failed-to-init"
#     
#         if(is_shiny == T) setProgress(0.05, paste("Analyzing", trap_data$conditions, trap_data$obs))
#         
#         
#         #mv2nm <- trap_data$mv2nm
#         nm2pn <- unique(trap_data$nm2pn)
#         project <- unique(trap_data$project)
#         conditions <- unique(trap_data$conditions)
#         date <- unique(trap_data$date)
#         obs <- unique(trap_data$obs)
#         
#         processed_data <- trap_data$processed[[1]]$bead
#         #processed_data <- trap_data$data[[1]]$processed_bead
#         
#         #### RUNNING MEAN & VAR ####
#         w_width <- 150
#         
#         if(is_shiny == T) setProgress(0.1, detail = "Calculating Running Windows")
#         
#         run_mean <- na.omit(RcppRoll::roll_meanl(processed_data, n = w_width, by = w_width/2))
#         #run_mean_to_subtract <- na.omit(RcppRoll::roll_meanl(processed_data, n = 50))
#         #processed_mean_removed <- processed_data[1:length(run_mean_to_subtract)] - run_mean_to_subtract
#         run_var <- na.omit(RcppRoll::roll_varl(processed_data, n = w_width, by = w_width/2))
#       
#        
#         #### HMM ####
#         
#         if(is_shiny == T) setProgress(0.2, detail = "HM-Model")
#         hm_model_results <- fit_hm_model(trap_data = trap_data, 
#                                          run_mean = run_mean, 
#                                          run_var = run_var, 
#                                          em_random_start = em_random_start,
#                                          is_shiny = F)
#         
#         #### MEASURE EVENTS ####
#        
#         conversion <- w_width/2
#         measured_hm_events <- measure_hm_events(hm_model_results = hm_model_results, 
#                                                 conversion = w_width/2, 
#                                                 hz = hz,
#                                                 nm2pn = nm2pn)
#         if(is_shiny == T) setProgress(0.2)
#       
#         
#         #### CHANGEPOINT ####
#      
#        
#        cp_data <- changepoint_and_ensemble(flip_raw = measured_hm_events$flip_raw,
#                                            hz = hz, 
#                                            conversion = w_width/2,
#                                            viterbi_rle = measured_hm_events$viterbi_rle,
#                                            hm_event_transitions  = measured_hm_events$hm_event_transitions, 
#                                            is_positive = measured_hm_events$is_positive,
#                                            write = F)
#       
#      
#         # dygraph_periods <- cbind(better_time_on, regroup_data) %>%
#         #   dplyr::mutate(state_2_start = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
#         #                                 state_1_end*conversion,
#         #                                 start),
#         #          state_2_stop = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
#         #                                state_2_end*conversion,
#         #                                stop)) %>%
#         #   dplyr::rename("run_from" = state_1_end,
#         #          "run_to" = state_2_end) %>%
#         #   dplyr::select(state_2_start, state_2_stop, run_from, run_to)
#         
#         
#         ###add better on times & displacements to final table
#         single_molecule_results <- measured_hm_events$measured_events_hm_estimates %>% 
#           dplyr::full_join(cp_data$cp_event_transitions) %>%
#           dplyr::mutate(final_time_ons_ms = ifelse(is.na(start) | is.na(stop)  | cp_time_on_dp <= 0,
#                                             time_on_ms,
#                                             cp_time_on_ms),
#                  final_displacements = ifelse(is.na(start)  | is.na(stop)  | cp_time_on_dp <= 0,
#                                               displacement_nm,
#                                               displacements),
#                  analyzer = 'hm-model/cp',
#                  hm_event_start = measured_hm_events$hm_event_transitions$state_1_end + 1,
#                  hm_event_stop = measured_hm_events$hm_event_transitions$state_2_end,
#                  cp_event_start_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
#                                         (hm_event_start - 1)*conversion,
#                                         start),
#                  cp_event_stop_dp = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
#                                        hm_event_stop*conversion,
#                                        stop),
#                  project = project,
#                  conditions = conditions,
#                  date = date, 
#                  obs = obs) %>%
#           dplyr::select(project, 
#                         conditions, 
#                         date, 
#                         obs, 
#                         time_off_ms, 
#                         final_time_ons_ms, 
#                         final_displacements,
#                         force, 
#                         analyzer,
#                         hm_event_start, 
#                         hm_event_stop,
#                         cp_event_start_dp,
#                         cp_event_stop_dp,
#                         conversion = conversion) %>% 
#           dplyr::rename("time_on_ms" = final_time_ons_ms,
#                  "displacement_nm" = final_displacements)   
#         
#        
#        ####EVENT FREQ####
#         
#         event_freq <- event_frequency(processed_data, measured_hm_events$viterbi_rle, conversion)
#         
#         ####Get Plot Data####
#         s1_avg_4plot <- tibble::tibble(avg = measured_hm_events$state_1_avg,
#                                        state_order = seq(from = 1, length.out = length(avg), by = 2))
#         
#         
#         s2_avg_4plot <- tibble::tibble(avg = ifelse(is.na(cp_data$absolute_displacements), 
#                                                     measured_hm_events$state_2_avg, 
#                                                     cp_data$absolute_displacements),
#                                       state_order = seq(from = 2, length.out = length(avg), by = 2))
#         
#         hmm_overlay <- bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
#           arrange(state_order)
# 
#         overlay <- unlist(map2(hmm_overlay$avg,
#                                measured_hm_events$viterbi_rle$lengths,
#                                ~rep(.x, times = conversion * .y)))
#         
#         overlay <- c(overlay, rep(overlay[length(overlay)], length(processed_data) - length(overlay)))
#         
#         
#         if(did_it_flip == FALSE){
#           mean_var_tib <- tibble(rm = hmm_identified_events$run_mean,
#                                  rv = hmm_identified_events$run_var,
#                                  state = paste('State', hmm_identified_events$state))
#         } else {
#           mean_var_tib <- tibble(rm = hmm_identified_events$run_mean*-1,
#                                  rv = hmm_identified_events$run_var,
#                                  state = paste('State', hmm_identified_events$state))
#         }
#         
#         
#         report_data  <- "success"
#         trap_data %<>% dplyr::mutate(hm_overlay = overlay,
#                                      report = report_data,
#                                      status = 'analyzed')
#         
#         if(is_shiny == T) setProgress(0.95, detail = 'Saving Data')
#         
#         file_names <-  c('trap-data.csv', 
#                          'measured-events.csv',
#                          'ensemble-data.csv',
#                          'hm-model-data.csv',
#                          'event-frequency.csv')
#         
#         file_paths <-  file.path('~/lasertrapr', conditions, date, obs, file_names)
#       
#         data_to_save <- list(trap_data,
#                              single_molecule_results,
#                              cp_data$ensemble_data,
#                              hm_model_results,
#                              event_freq)
#         
#         walk2(data_to_save, file_paths, ~readr::write_csv(x = .x, path = .y))
#         
#       
#        
#         
#         # data_to_save <- single_molecule_results %>% 
#         #   dplyr::mutate() %>% 
#         #   dplyr::select(time_off_ms, final_time_ons_ms,  final_displacements, force, analyzer) %>%
#         
# 
#         
#     #     if(last_state == 2){
#     #       
#     #       overlay <- vector()
#     #       for(i in 1:(nrow(hmm_overlay)-1)){
#     #         
#     #         overlay[i] <- rep(hmm_overlay$avg[i],
#     #                           (conversion*rle_object$lengths[-length(rle_object$lengths)][i]))
#     #       }
#     #     } else {
#     #       
#     #       overlay <- vector('list')
#     #       for(i in seq_along(1:nrow(hmm_overlay))){
#     #         overlay[[i]] <- rep(hmm_overlay$avg[i],
#     #                             (conversion*rle_object$lengths[-length(rle_object$lengths)][i]))
#     #       }
#     #     }
#     #     
#     # overlay <- unlist(overlay)
#         
#    #   dy_rv <- tibble(window = 1:nrow(hmm_identified_events),
#    #                      rv = hmm_identified_events$run_var)
#    #   dy_rm <- tibble(Window = 1:nrow(hmm_identified_events),
#    #                      rm = hmm_identified_events$run_mean)
#    #   shades_df <- data.frame(start = dygraph_periods$run_from,
#    #                              stop = dygraph_periods$run_to)
#    #      
#    #   shade_col <- '#E2E2E2'
#    #   
#    #   color <- c(pink(), purple())
#    #        
#    #   rv_dy <- dygraphs::dygraph(dy_rv, group = 'group') %>%
#    #              dygraphs::dySeries('rv', color = color[[1]], strokeWidth = 2) %>%
#    #              dygraphs::dyAxis('x', axisLineColor = '#FFFFFF', drawGrid = FALSE, axisLabelColor = '#FFFFFF') %>%
#    #              dygraphs::dyAxis('y', label = 'Running Variance', drawGrid = FALSE,) %>%
#    #              add_shades(periods = shades_df, color = shade_col) %>%
#    #              dygraphs::dyUnzoom()
#    # 
#    #        
#    #   rm_dy <- dygraphs::dygraph(dy_rm, group = 'group') %>%
#    #              dygraphs::dySeries('rm', color = color[[2]],  strokeWidth = 2) %>%
#    #              dygraphs::dyAxis('x', label = 'Window', drawGrid = FALSE) %>%
#    #              dygraphs::dyAxis('y', label = 'Running Mean (nm)', drawGrid = FALSE) %>%
#    #              add_shades(periods = shades_df, color = shade_col) %>%
#    #              dygraphs::dyRangeSelector(fillColor ='white', strokeColor = color[[1]])
#    #   
#    #   if(is_shiny == T) setProgress(0.77, detail = "Plotting something...")
#    #   d <- data.frame(index = (1:length(overlay)/5000),
#    #                  raw = flip_raw[1:length(overlay)],
#    #                  model = overlay)
#    # 
#    #  periods_df <- data.frame(start = dygraph_periods$state_2_start/5000,
#    #                           stop = dygraph_periods$state_2_stop/5000)
#    # 
#    #  pni <- ((peak_nm_index * conversion) - 75)/5000
#    #    
#    # overlay_dy <-  dygraphs::dygraph(d) %>% #raw_data_dygraph
#    #                  dygraphs::dySeries('raw', color = '#242424', strokeWidth = 2) %>%
#    #                  dygraphs::dySeries('model', color = pink(),  strokeWidth = 2) %>%
#    #                  dygraphs::dyRangeSelector(fillColor ='white', strokeColor = color[[1]]) %>%
#    #                  add_shades(periods_df, color = '#ffd2df') %>% #raw_periods
#    #                  add_labels_hmm(measured_events, peak_nm_index = pni, labelLoc = 'bottom') %>% #results$events
#    #                  dygraphs::dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
#    #                  dygraphs::dyAxis('y', label = 'nm') %>%
#    #                  dygraphs::dyUnzoom()
# 
#  
#     #
#     #
#     # Plots of the running mean vs. running variance.
#     # This provides insight into how the model divided data into either the baseline or event populations.
#     # echo=FALSE, message = FALSE, fig.width = 16, fig.height = 6
#    
#   
#     
#     
#     # mv1 <- ggplot2::ggplot(mean_var_tib)+
#     #   geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
#     #   scale_color_manual(values = c(pink(), purple()))+
#     #   ggtitle('Mean-Variance (overlayed)')+
#     #   ylab('Running Variance (nm)')+
#     #   xlab('Running Mean (nm)')+
#     #   theme_black(base_size = 18)+
#     #   theme(legend.position = 'none')
#     #  
#     # mv2 <- ggplot(mean_var_tib)+
#     #   geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
#     #   scale_color_manual(values = c(pink(), purple()))+
#     #   facet_wrap(~state)+
#     #   ggtitle('Mean-Variance (by state)')+
#     #   ylab('')+
#     #   xlab('Running Mean (nm)')+
#     #   theme_black(base_size = 18)
#     # 
#     # mv_state_plot <- gridExtra::grid.arrange(mv1, mv2, nrow = 1)
#     # 
#     # if(is_shiny == T) setProgress(0.8, detail = "Calculating event frequency")
# 
# 
#         
#       
#     
#         report_data <- "event_freq_fail"
#         #### EVENT FREQUENCY ####
#      
#                  
#         
#         results_data <- list(events = measured_events,
#                              ensemble_avg = ensemble_avg_df,
#                              event_freq = event_freq$data)
#         
#     
#         if(is_shiny == T) setProgress(0.9, detail = "Saving Data")            
#           
#         report_data <- "success"
#         obs_trap_data <- trap_data[folder, ] %>% 
#           dplyr::mutate(results = list(results_data),
#                         report = report_data,
#                         status = 'analyzed')
#         
#         
#        saveRDS(obs_trap_data, file = trap_data$rds_file_path)
#         
#        if(is_shiny == T) setProgress(0.95, detail = "Saving Viz")    
#         
#        plot_data <- list( events = measured_events,
#                            peak_nm_index = (peak_nm_index * conversion)/5000,
#                            shades_df = shades_df,
#                            raw_periods = periods_df, 
#                            event_freq_plot = event_freq$plot,
#                            s2n = var_signal_to_noise,
#                            mv_state_plot = mv_state_plot,
#                            hmm_fit = hmm_fit,
#                            rv_dy = rv_dy,
#                            rm_dy = rm_dy,
#                           hm_model_data = hmm_identified_events,
#                            overlay_dy = overlay_dy )
#         
#                 
#         viz <- trap_data[folder, ] %>% 
#           dplyr::select(project, conditions, date, obs) %>%  
#           dplyr::mutate(plot = list(plot_data))
#         
#         viz_path <- str_replace(trap_data$rds_file_path, 'trap-data.rds', 'viz.rds')
#         saveRDS(viz, file = viz_path)
#         #summary(hmm_fit) will gve hmm model == sum_fit object
#         #measured events
#         
#         if(is_shiny == T) setProgress(0.97, 'Done with this one')
#         
# 
#       }, error=function(e){
#         showNotification(paste0("Analysis error in ",
#                                 trap_data$date,
#                                 " ",
#                                 trap_data$conditions,
#                                 " ",
#                                 trap_data$obs,
#                                 ". Error Message: ",
#                                 as.character(e)), type = 'warning', duration = NULL)
#         writeLines(paste0("Analysis error in ",
#                           trap_data$date,
#                           " ",
#                           trap_data$conditions,
#                           " ",
#                           trap_data$obs,
#                           ". Error Message: ",
#                           as.character(e)), error_file)
#       })
#       
#     }
#     
#     close(error_file)
#     if(is_shiny == T) setProgress(1, detail = "Done!")
#     
# }
# 
# 
# 
# 
# 
# 
