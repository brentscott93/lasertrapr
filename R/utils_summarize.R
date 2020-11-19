#' Prep signficance table using Krusial-Wallis and Wilcox
#'
#' @param data From measured-events.csv
#' @param conditions Grouping column
#' @param y.trans One of rstatix::add_xy_position y.trans options
#'
#' @return a list
#' @export
#'
kruskal_wilcox <- function(data, conditions, y.trans){
  
kruskal <-  data %>% 
  kruskal_test(time_on_ms ~ conditions)

wilcox <-  data %>% 
  wilcox_test(time_on_ms ~ conditions,  p.adjust.method = "bonferroni") 

is_sig <- which(wilcox$p.adj < 0.05)

table <- wilcox %>% 
  dplyr::select(group1, group2, p.adj) %>% 
  ggpubr::ggtexttable (theme = ttheme(base_style = 'light'), rows = NULL) %>% 
  ggpubr::table_cell_bg(row = ton_is_sig + 1, column = 3, fill = '#ffa9a6')

wilcox %<>% add_xy_position(x = "conditions2", y.trans = y.trans)

list(kruskal = kruskal,
     wilcox = wilcox,
     table = table)
}

compare_displacements <- function(event_files_filtered, plot_colors){
  step_histo <- ggplot(data = event_files_filtered,
                       aes(x = displacement_nm,
                           fill = conditions))+
    geom_histogram(aes(y = stat(density)),
                   binwidth = 3,
                   color = "black",
                   size = 0.9)+
    facet_wrap(~conditions, ncol = 2)+
    xlab("Step Size (nm)")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(breaks = seq(-100, 100, by = 10))+
   # scale_fill_manual(values = plot_colors)+
    #scale_fill_brewer(palette = "Dark2")+
    theme_linedraw(base_size = 11)+
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  step_aov <-  event_files_filtered %>%
    rstatix::anova_test(displacement_nm ~ conditions)
  
  step_tukey <-  event_files_filtered %>%
    stats::aov(displacement_nm ~ conditions, data = .) %>%
    rstatix::tukey_hsd()
  
  step_is_sig <- which(step_tukey$p.adj < 0.05)
  
  step_table <- step_tukey %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9),
                         rows = NULL)
  
  any_sig <- step_tukey %>% 
    dplyr::filter(p.adj < 0.05) %>% 
    nrow()
  
  if(any_sig > 0){
    step_table <-   ggpubr::table_cell_bg(step_table, row = step_is_sig + 1, column = 3, fill = '#ffa9a6')
  }
  
  step_tukey <- rstatix::add_xy_position(step_tukey, 
                                         data = event_files_filtered, 
                                         formula = displacement_nm ~ conditions)
  
  
  step_violin <- ggpubr::ggviolin(event_files_filtered,
                                  x = "conditions",
                                  y = "displacement_nm",
                                  fill = "conditions",
                                 # palette = plot_colors,
                                  add = "boxplot",
                                  xlab = '',
                                  ylab = 'Displacement (nm)',
                                  ggtheme = ggpubr::theme_pubr(base_size=11))+
    ggpubr::stat_pvalue_manual(step_tukey, 
                               bracket.nudge.y = seq(10, by = 4, length.out = any_sig),
                               hide.ns = T,
                               bracket.size = 0.9,
                               tip.length = 0.01)+
    scale_y_continuous(breaks = seq(-50, 1000, by = 10))+
   # rotate_x_text(angle = 45)+
    # rotate()+
    #yscale('log10', .format = T)+
    labs(
      subtitle = rstatix::get_test_label(step_aov, detailed = TRUE),
      caption = rstatix::get_pwc_label(step_tukey)
    ) +
    theme(legend.position = 'none')
  
  list(histogram = step_histo,
       violin = step_violin,
       table = step_table)
  
  
}


#' Title
#'
#' @param f 
#'
#' @return
#' @export
#' @import data.table
#' @examples
summarize_trap_data <- function(f){
  
  trap_selected_project <- f$project$path
  #browser()
  trap_data_paths <- list_files(trap_selected_project,
                                pattern = "trap-data.csv",
                                recursive = TRUE)
  
  setProgress(0.05, detail = 'Reading Data')
  # trap_data <- purrr::map(trap_data_paths$path, function(x){ incProgress(0.0025)
  #   data.table::fread(x)} )
  
  trap_data <- purrr::map(trap_data_paths$path, data.table::fread, nrows = 1) %>% 
    data.table::rbindlist(fill = T) %>% 
    .[report == "success" & review == T & include == T]
  
  
  setProgress(0.1, detail = 'Getting event paths')
  event_file_paths <- unique(trap_data[, .(project, conditions, date, obs)]) %>% 
    unite('path', c(project, conditions, date, obs), sep = "/") %>% 
    pull(path) %>% 
    paste0("~/lasertrapr/", ., '/measured-events.csv')
  
  event_files_filtered <- lapply(event_file_paths, data.table::fread) %>%
    data.table::rbindlist(fill = T) %>% 
    .[keep == T] 
  
  event_files_filtered$conditions <- factor(data$event_files_filtered$conditions,
                                                 levels = input$factor_order)
  
  get_time <-  purrr::map(trap_data_paths$path, function(x){ incProgress(0.0025)
                                                   data.table::fread(x, 
                                                                     select = c('conditions', 'report', 'review', 'include'))} ) %>%
    data.table::rbindlist(fill = T) %>% 
    .[report == "success" & review == T & include == T, .N, by = conditions] %>% 
    dplyr::mutate(minutes = round((N/5000)/60, 2))
  
  summarize_trap <- event_files_filtered %>%
    dplyr::group_by(conditions) %>%
    dplyr::summarize(time_on_avg = mean(time_on_ms),
                     time_on_se = plotrix::std.error(time_on_ms, na.rm = TRUE),
                     time_on_median = median(time_on_ms, na.rm = TRUE),
                     time_off_avg = mean(time_off_ms, na.rm = TRUE),
                     time_off_se = plotrix::std.error(time_off_ms, na.rm = TRUE),
                     time_off_median = median(time_off_ms, na.rm = TRUE), 
                     displacement_avg = mean(displacement_nm, na.rm = TRUE),
                     displacement_se = plotrix::std.error(displacement_nm, na.rm = TRUE),
                     force_avg = mean(force, na.rm = TRUE),
                     force_se = plotrix::std.error(force, na.rm = TRUE),
                     trap_stiffness = mean(trap_stiffness, na.rm = T),
                     myo_stiffness = mean(myo_stiffness, na.rm = T),
                     num_events = n()) %>%
    dplyr::right_join(get_time)# %>% 
  
  return(event_files_filtered = event_files_filtered,
         summary = summarize_trap,
         event_file_paths = event_file_paths)
}