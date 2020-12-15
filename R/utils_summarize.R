
#' Summarize trap project
#'
#' @param f reactive folder list
#' @param hz sampling frequency
#' @param factor_order user supplied order of factors
#'
#' @return
#' @export
#'
#' @examples
summarize_trap_data <- function(f, hz, factor_order){
  
  trap_selected_project <- f$project$path
  #browser()
  trap_data_paths <- 
    list_files(trap_selected_project,
               pattern = "trap-data.csv",
               recursive = TRUE)
  
  setProgress(0.05, detail = 'Reading Data')
  # trap_data <- purrr::map(trap_data_paths$path, function(x){ incProgress(0.0025)
  #   data.table::fread(x)} )
  
  trap_data <-
    lapply(trap_data_paths$path, data.table::fread, nrows = 1) %>% 
    data.table::rbindlist(fill = T) %>% 
    dplyr::filter(report == "success" & review == T & include == T)
  
  
  event_file_paths <- 
    unique(trap_data[, .(project, conditions, date, obs)]) %>% 
    unite('path', c(project, conditions, date, obs), sep = "/") %>% 
    pull(path) %>% 
    paste0("~/lasertrapr/", ., '/measured-events.csv')
  
  event_files_filtered <- 
    lapply(event_file_paths, data.table::fread) %>%
    data.table::rbindlist(fill = T) %>% 
    dplyr::filter(keep == T)
  
  event_files_filtered$conditions <- 
    factor(event_files_filtered$conditions,
           levels = factor_order)
  
  get_time <- 
    lapply(trap_data_paths$path,
           function(x){ 
             incProgress(0.0025)
              data.table::fread(x, 
                                select = c('conditions',
                                           'report', 
                                           'review',
                                           'include'))} ) %>%
              data.table::rbindlist(fill = T) %>% 
              .[report == "success" & review == T & include == T, .N, by = conditions] %>% 
              dplyr::mutate(minutes = round((N/hz)/60, 2))
  
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
  
  return(list(event_files_filtered = event_files_filtered,
               summary = summarize_trap,
               event_file_paths = event_file_paths))
}

stats_plot_step <- function(event_files_filtered, plot_colors){
  
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
    scale_fill_manual(values = plot_colors)+
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
    ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL)
  
  if(any(step_is_sig))  step_table %<>% ggpubr::table_cell_bg(row = step_is_sig + 1, column = 3, fill = '#ffa9a6')
  
  step_tukey <- rstatix::add_xy_position(step_tukey, data = event_files_filtered, formula = displacement_nm ~ conditions)
  
  step_violin <- ggpubr::ggviolin(event_files_filtered,
                                  x = "conditions",
                                  y = "displacement_nm",
                                  fill = "conditions",
                                  palette = plot_colors,
                                  add = "boxplot",
                                  xlab = '',
                                  ylab = 'Displacement (nm)',
                                  ggtheme = ggpubr::theme_pubr(base_size=11))+
    ggpubr::stat_pvalue_manual(step_tukey, bracket.nudge.y = seq(10, by = 4, length.out = nrow(step_tukey)))+
    scale_y_continuous(breaks = seq(-50, 1000, by = 15))+
    rotate_x_text(angle = 45)+
    # rotate()+
    #yscale('log10', .format = T)+
    labs(
      subtitle = rstatix::get_test_label(step_aov, detailed = TRUE),
      caption = rstatix::get_pwc_label(step_tukey)
    ) +
    theme(legend.position = 'none')
  
  
  
  #  step_top <- cowplot::plot_grid(step_violin, step_histo, align = "hv", axis = "bt", rel_widths = c(1,1))
  step_side <- cowplot::plot_grid(step_histo, step_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  step_main <-  cowplot::plot_grid(step_violin, step_side, ncol = 2, nrow = 1,  rel_heights = c(1, 1))
  # now add the title
  step_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Displacement Distributions, ANOVA, & Tukey Table",
      fontface = 'bold',
      size = 18,
      x = 0.5
    )
  
  cowplot::plot_grid(step_title, 
                     step_main, #step_table,
                     ncol = 1, 
                     nrow =2,
                     rel_heights = c(0.05, 1))
  
}

stats_plot_time_on <- function(event_files_filtered, plot_colors){
  
  ton_histo <- ggplot(data = event_files_filtered,
                      aes(x = time_on_ms,
                          fill = conditions))+
    geom_histogram(aes(y = stat(density)),
                   binwidth = 20,
                   color = "black",
                   size = 0.9)+
    facet_wrap(~conditions, ncol = 2)+
    xlab("Time on (ms)")+
    scale_y_continuous(expand = c(0,0))+
    #scale_x_continuous(breaks = seq(0, 6000, by = 100))+
    coord_cartesian(c(0, 500))+
    scale_fill_manual(values = plot_colors)+
    #scale_fill_brewer(palette = "Dark2")+
    theme_linedraw(base_size = 11)+
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  ton_kruskal <-  event_files_filtered %>%
    rstatix::kruskal_test(time_on_ms ~ conditions)
  
  ton_wilcox <-   event_files_filtered %>%
    rstatix::wilcox_test(time_on_ms ~ conditions,  p.adjust.method = "holm")
  
  ton_is_sig <- which(ton_wilcox$p.adj < 0.05)
  
  ton_table <- ton_wilcox %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL)
  
  if(any(ton_is_sig)) ton_table %<>% ggpubr::table_cell_bg(row = ton_is_sig + 1, column = 3, fill = '#ffa9a6')
  
  ton_wilcox %<>% rstatix::add_xy_position(x = "conditions", y.trans = log10)
  
  ton_violin <- ggpubr::ggviolin(event_files_filtered,
                                 x = "conditions",
                                 y = "time_on_ms",
                                 fill = "conditions",
                                 palette = plot_colors,
                                 add = "boxplot",
                                 xlab = '',
                                 ylab = 'Time On (ms)',
                                 ggtheme = ggpubr::theme_pubr(base_size=11))+
    ggpubr::stat_pvalue_manual(ton_wilcox, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(ton_wilcox)))+
    yscale('log10', .format = T)+
    rotate_x_text(angle=45)+
    labs(
      subtitle = rstatix::get_test_label(ton_kruskal, detailed = TRUE),
      caption = rstatix::get_pwc_label(ton_wilcox)
    ) +
    theme(legend.position = 'none')
  
  ton_side <- cowplot::plot_grid(ton_histo, ton_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  ton_main <-  cowplot::plot_grid(ton_violin, ton_side, ncol = 2, nrow = 1)
  
  # now add the title
  ton_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Time On Distributions, Kruskal-Wallis, & Pairwise Wilcox Table",
      fontface = 'bold',
      size = 18,
      x = 0.5
    )
  
  cowplot::plot_grid(ton_title,
                     ton_main, #ton_table,
                     ncol = 1,
                     nrow =2,
                     rel_heights = c(0.05, 1))
}

stats_plot_time_off <- function(event_files_filtered, plot_colors){
  toff_histo <- 
    ggplot(data = event_files_filtered,
           aes(x = time_off_ms,
               fill = conditions))+
    geom_histogram(aes(y = stat(density)),
                   binwidth = 100,
                   color = "black",
                   size = 0.9)+
    facet_wrap(~conditions, ncol=2)+
    xlab("Time Off (ms)")+
    scale_y_continuous(expand = c(0,0))+
    #scale_x_continuous(breaks = seq(0, 20000, by = 500))+
    coord_cartesian(c(0, 2000))+
    scale_fill_manual(values = plot_colors)+
    #scale_fill_brewer(palette = "Dark2")+
    theme_linedraw(base_size = 11)+
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  
  toff_kruskal <-  
    event_files_filtered %>%
    rstatix::kruskal_test(time_off_ms ~ conditions)
  
  toff_wilcox <-   
    event_files_filtered %>%
    wilcox_test(time_off_ms ~ conditions,  p.adjust.method = "holm")
  
  toff_is_sig <- which(toff_wilcox$p.adj < 0.05)
  
  toff_table <- 
    toff_wilcox %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggpubr::ggtexttable (theme = ttheme(base_style = 'light', base_size = 9), rows = NULL)
  
  if(any(toff_is_sig)) toff_table %<>%  ggpubr::table_cell_bg(row = toff_is_sig + 1, column = 3, fill = '#ffa9a6')
  
  toff_wilcox %<>% add_xy_position(x = "conditions", y.trans = log10)
  
  toff_violin <- 
    ggviolin(event_files_filtered,
             x = "conditions",
             y = "time_off_ms",
             fill = "conditions",
             palette = plot_colors,
             add = "boxplot",
             xlab = '',
             ylab = 'Time Off (ms)',
             ggtheme = theme_pubr(base_size=11))+
    stat_pvalue_manual(toff_wilcox, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(toff_wilcox)))+
    yscale('log10', .format = T)+
    rotate_x_text(angle=45)+
    labs(
      subtitle = get_test_label(toff_kruskal, detailed = TRUE),
      caption = get_pwc_label(toff_wilcox)
    ) +
    theme(legend.position = 'none')
  
  
  toff_side <- cowplot::plot_grid(toff_histo, toff_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  toff_main <-  cowplot::plot_grid(toff_violin, toff_side, ncol = 2, nrow = 1)
  
  # now add the title
  toff_title <- ggdraw() +
    draw_label(
      "Time Off Distributions, Kruskal-Wallis, & Pairwise Wilcox Table",
      fontface = 'bold',
      size = 18,
      x = 0.5
    )
  
  plot_grid(toff_title, 
             toff_main, #toff_table,
             ncol = 1,
             nrow =3,
             rel_heights = c(0.05, 1))
}

stats_plot_force <- function(event_files_filtered, plot_colors){
  
  force_histo <- ggplot(data = event_files_filtered,
                        aes(x = force,
                            fill = conditions))+
    geom_histogram(aes(y = stat(density)),
                   binwidth = 0.1,
                   color = "black",
                   size = 0.9)+
    facet_wrap(~conditions, ncol=2)+
    xlab("Force (pN)")+
    scale_y_continuous(expand = c(0,0))+
    #scale_x_continuous(breaks = seq(0, 20000, by = 500))+
    scale_fill_manual(values = plot_colors)+
    #scale_fill_brewer(palette = "Dark2")+
    theme_linedraw(base_size = 11)+
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  
  force_aov <-  event_files_filtered %>%
    anova_test(force ~ conditions)
  
  force_tukey <-  event_files_filtered %>%
    aov(force~conditions, data = .) %>%
    rstatix::tukey_hsd()
  
  force_is_sig <- which(force_tukey$p.adj < 0.05)
  
  force_table <- force_tukey %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL)
  
  if(any(force_is_sig)) force_table %<>% ggpubr::table_cell_bg(row = force_is_sig + 1, column = 3, fill = '#ffa9a6')
  
  force_tukey <- add_xy_position(force_tukey, data = event_files_filtered, formula = force ~ conditions)
  
  force_violin <- ggviolin(event_files_filtered,
                           x = "conditions",
                           y = "force",
                           fill = "conditions",
                           palette = plot_colors,
                           add = "boxplot",
                           xlab = '',
                           ylab = 'Time Off (ms)',
                           ggtheme = theme_pubr(base_size=11))+
    stat_pvalue_manual(force_tukey, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(force_tukey)))+
    scale_y_continuous(breaks = seq(-3, 5, by = 1))+
    rotate_x_text(angle=45)+
    #yscale('log10', .format = T)+
    labs(
      subtitle = get_test_label(force_aov, detailed = TRUE),
      caption = get_pwc_label(force_tukey)
    ) +
    theme(legend.position = 'none')
  
  force_side <- cowplot::plot_grid(force_histo, force_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  force_main <-  cowplot::plot_grid(force_violin, force_side, ncol = 2, nrow = 1)
  
  force_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Force Distributions, ANOVA, & Tukey Table",
      fontface = 'bold',
      size = 18,
      x = 0.5
    )
  
  cowplot::plot_grid(force_title,
                     force_main, #force_table,
                     ncol = 1,
                     nrow =2,
                     rel_heights = c(0.05, 1))
}

#' Time On Survival Analysis
#'
#' @param event_files_filtered 
#' @param plot_colors 
#' @return
#' @export
#'
#' @noRd
#' @import survminer survival patchwork
survival_analysis <- function(event_files_filtered, plot_colors){
  
  fit <- survival::survfit(survival::Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  
  ggsurvival <-  
    survminer::ggsurvplot(fit,
                          data = event_files_filtered,
                          ggtheme = theme_linedraw(base_size = 12), # Change ggplot2 theme 
                          palette = plot_colors,
                          xlim = c(0, 1000),
                          break.time.by = 150,
                          fun = 'event'
                           # pval = T
  )
  
  all_surv <- ggsurvival$plot +
    xlab('Time (ms)')+
    xlab("")+
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
    )
  
  facet_surv <- ggsurvival$plot +
    facet_wrap(~conditions)+
    xlab('Time (ms)')+
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = 'none')
  
  ton_survival_plots <- all_surv/facet_surv
  ton_log_rank_test <- survival::survdiff(survival::Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  ton_cox <- survival::coxph(survival::Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  
  list(ton_survival_plots = ton_survival_plots,
       ton_log_rank_test = ton_log_rank_test,
       ton_cox = ton_cox)
       
}

stats_plot_event_frequency <- function(event_file_paths, factor_order, plot_colors){
  ef_paths <- str_replace(event_file_paths, "measured-events", "event-frequency")
  
  ef_read <- function(path){
    data.table::fread(path) %>%
      dplyr::mutate(path = path)
  }
  ef <- map_df(ef_paths, ef_read) %>%
    separate(path, c('root', 'id_info'), sep = '/project_') %>% 
    separate(id_info, c("project", 'conditions', 'date', 'obs', 'file'), sep = '/')
  
  ef$conditions <- factor(ef$conditions, levels = factor_order)
  
  ef_kruskal <- ef %>% rstatix::kruskal_test(freq_start ~ conditions)
  
  ef_wilcox <-  ef %>%
    wilcox_test(freq_start ~ conditions, p.adjust.method = "holm")
  
  ef_is_sig <- which(ef_wilcox$p.adj < 0.05)
  
  ef_table <- ef_wilcox %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggtexttable (theme = ttheme(base_style = 'light', base_size = 10), rows = NULL) 
  
  if(any(ef_is_sig)) ef_table %<>%table_cell_bg(row = ef_is_sig + 1, column = 3, fill = '#ffa9a6')
  
  ef_wilcox %<>% add_xy_position(x = "conditions")
  
  ef_plot <- ggplot(data = ef, aes(conditions, freq_start))+
    geom_jitter(aes(color = conditions), size = 2, alpha = 0.8)+
    stat_pvalue_manual(ef_wilcox) +
    stat_summary(
      fun.data = mean_sdl,  fun.args = list(mult = 1),
      geom = "pointrange", color = "black"
    )+
    scale_color_manual(values = plot_colors)+
    scale_y_continuous(breaks = 0:15)+
    xlab('')+
    ylab('Events/sec')+
    labs(
      title = 'Event Frequency',
      subtitle = get_test_label(ef_kruskal, detailed = TRUE),
      caption = get_pwc_label(ef_wilcox)
    )+
    theme_cowplot(font_size = 11)+
    theme(legend.position = 'none')#+

  ef_plot <- plot_grid(ef_plot, ef_table, ncol = 2, nrow = 1, rel_widths = c(2, 1))
  
  #no zero
  ef_no0 <- dplyr::filter(ef, freq_start != 0 )
  ef_kruskal_no_zero <- ef_no0 %>%
    kruskal_test(freq_start ~ conditions)

  ef_wilcox_no_zero <-  ef_no0 %>%
    wilcox_test(freq_start ~ conditions, p.adjust.method = "holm")

  ef_is_sig_no_zero <- which(ef_wilcox_no_zero$p.adj < 0.05)

  ef_table_no_zero <- ef_wilcox_no_zero %>%
    dplyr::select(group1, group2, p.adj) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light'), rows = NULL) 
  
  if(any(ef_is_sig_no_zero)) ef_table_no_zero %<>% table_cell_bg(row = ef_is_sig_no_zero + 1, column = 3, fill = '#ffa9a6')

  ef_wilcox_no_zero %<>% add_xy_position(x = "conditions")

  ef_plot_no_zero <- ggplot(data = ef_no0, aes(conditions, freq_start))+
    geom_jitter(aes(color = conditions), size = 2, alpha = 0.8)+
    stat_pvalue_manual(ef_wilcox_no_zero) +
    stat_summary(
      fun.data = mean_sdl,  fun.args = list(mult = 1),
      geom = "pointrange", color = "black"
    )+
    scale_color_manual(values = plot_colors)+
    scale_y_continuous(breaks = 0:15)+
    xlab('')+
    ylab('Events/sec')+
    labs(
      title = 'Event Frequency (zero counts eliminated)',
      subtitle = get_test_label(ef_kruskal_no_zero, detailed = TRUE),
      caption = get_pwc_label(ef_wilcox_no_zero)
    )+
    theme_cowplot(font_size = 11)+
    theme(legend.position = 'none')#+

  ef_plot_no_zero <- plot_grid(ef_plot_no_zero, ef_table_no_zero, ncol = 2, nrow = 1, rel_widths = c(1, 0.4))
  
  list(ef_plot = ef_plot,
       ef_plot_no_zero = ef_plot_no_zero)
}

correlations <- function(event_files_filtered, plot_colors){
  step_vs_on <- ggscatter(event_files_filtered,
                          y = "time_on_ms", 
                          x = "displacement_nm",
                          color = 'conditions',
                          palette = plot_colors, 
                          alpha = 0.25,
                          add = "reg.line",  # Add regressin line
                          add.params = list(color = "conditions", fill = "grey", size = 1.25), # Customize reg. line
                          conf.int = TRUE,
  )+
    facet_wrap(~conditions, ncol = 1)+
    scale_y_log10()+
    scale_x_continuous(breaks = seq(-100, 100, by = 10))+
    ylab('Time On (ms)')+
    xlab('Displacement (nm)')+
    ggtitle('Displacement vs Time On')+
    stat_cor(method = "pearson", label.x.npc = 0.6)+
    theme_linedraw(base_size = 14)+
    theme(panel.grid  = element_blank(),
          legend.position = 'none')
  
  
  
  on_vs_off <- ggscatter(event_files_filtered,
                         y = "time_on_ms", 
                         x = "time_off_ms",
                         color = 'conditions',
                         palette = plot_colors, 
                         alpha = 0.25,
                         #  facet.by = "conditions",
                         add = "reg.line",  # Add regressin line
                         add.params = list(color = "conditions", fill = "grey", size = 1.25), # Customize reg. line
                         conf.int = TRUE,
  )+
    facet_wrap(~conditions, ncol = 1)+
    scale_y_log10()+
    scale_x_log10()+
    ylab('Time On (ms)')+
    xlab('Time Off (ms)')+
    ggtitle('Time On vs Time Off')+
    stat_cor(method = "pearson", label.x.npc = 0.6)+
    theme_linedraw(base_size = 14)+
    theme(panel.grid  = element_blank(),
          legend.position = 'none')
  
  
  step_vs_off <- ggscatter(event_files_filtered,
                           x = "displacement_nm", 
                           y = "time_off_ms",
                           color = 'conditions',
                           alpha = 0.25,
                           palette = plot_colors, 
                           #facet.by = "conditions",
                           add = "reg.line",  # Add regressin line
                           add.params = list(color = "conditions", fill = "grey", size = 1.25), # Customize reg. line
                           conf.int = TRUE,
  )+
    facet_wrap(~conditions, ncol = 1)+
    scale_y_log10()+
    scale_x_continuous(breaks = seq(-100, 100, by = 10))+
    xlab('Displacement (nm)')+
    ylab('Time Off (ms)')+
    ggtitle('Displacement vs Time Off')+
    stat_cor(method = "pearson", label.x.npc = 0.6)+
    theme_linedraw(base_size = 14)+
    theme(panel.grid  = element_blank(),
          legend.position = 'none')
  
  
  ktrap_vs_kmyo <- ggscatter(event_files_filtered,
                             x = "trap_stiffness", 
                             y = "myo_stiffness",
                             color = 'conditions',
                             alpha = 0.25,
                             palette = plot_colors,
                             # facet.by = "conditions",
                             add = "reg.line",  # Add regressin line
                             add.params = list(color = "conditions", fill = "grey", size = 1.25), # Customize reg. line
                             conf.int = TRUE,
  )+
    facet_wrap(~conditions, ncol = 1)+
    xlab(expression(k[trap]))+
    ylab(expression(k[myo]))+
    ggtitle('ktrap vs kmyo')+
    scale_x_log10()+
    stat_cor(method = "pearson", label.x.npc = 0.6)+
    theme_linedraw(base_size = 14)+
    theme(panel.grid  = element_blank(),
          legend.position = 'none')
  
  step_vs_kmyo <- ggscatter(event_files_filtered,
                            y = "displacement_nm", 
                            x = "myo_stiffness",
                            color = 'conditions',
                            alpha = 0.25,
                            palette = plot_colors, 
                            # facet.by = "conditions",
                            add = "reg.line",  # Add regressin line
                            add.params = list(color = "conditions", fill = "grey", size = 1.25), # Customize reg. line
                            conf.int = TRUE,
  )+
    facet_wrap(~conditions, ncol = 1)+
    ylab('Displacement (nm)')+
    xlab(expression(k[myo]))+
    scale_y_continuous(breaks = seq(-100, 100, by = 10))+
    ggtitle('Displacement vs kmyo')+
    #scale_x_log10()+
    stat_cor(method = "pearson", label.x.npc = 0.6)+
    theme_linedraw(base_size = 14)+
    theme(panel.grid  = element_blank(),
          legend.position = 'none')
  
  
  plot_grid(step_vs_on, on_vs_off, step_vs_kmyo, ktrap_vs_kmyo, step_vs_off, nrow = 1) 
}

stiffness <- function(event_files_filtered, plot_colors){
  
  stiff_data <- event_files_filtered %>% 
    dplyr::select(myo_stiffness, trap_stiffness, conditions) %>% 
    tidyr::pivot_longer(cols= c(myo_stiffness, trap_stiffness), names_to = 'stiffness')
  
  stiff_data$stiffness <- factor(stiff_data$stiffness, levels = c('trap_stiffness', 'myo_stiffness'))
  mean_trap <- stiff_data %>% 
    group_by(conditions, stiffness) %>% 
    dplyr::summarize(mean_val = mean(value, na.rm = T),
                     sd = sd(value, na.rm = T)) %>% 
    mutate(s2n = ifelse(stiffness == 'trap_stiffness', '1', round(max(mean_val)/min(mean_val), 2)),
           label = paste0("\u03bc = ", round(mean_val, 3)))
  
  
  g <- ggplot(stiff_data)+
    geom_density(aes(value, fill = stiffness), alpha = 0.4,
                 #color = 'blac k'
    )+
    # geom_vline(data = mean_trap, aes(xintercept=mean_val))+
    scale_y_continuous(breaks = NULL, expand = expansion(c(0, 0)))+
    # scale_x_continuous(breaks = seq(0, 5, by = 0.05))+
    scale_fill_manual(values = c('blue', 'red'), 
                      name = '',
                      labels = c(bquote('k'[trap]), bquote('k'[myo])))+
    xlab('Stiffness (pN/nm)')+
    facet_wrap(~conditions)+
    theme_linedraw()+
    theme(legend.position = 'bottom',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  #g
  gb <- ggplot_build(g)
  
  #get_y <- gb$data[[1]] %>% filter(abs(x - mean_val) == min(abs(x - mean_val)) %>% pull(y)
  # mean_trap %<>%
  #   arrange(conditions) %>%
  #   mutate(gg_build = split(gb$data[[1]], gb$data[[1]]$PANEL))
  
  
  by_conditions <- split(gb$data[[1]], gb$data[[1]]$PANEL)
  by_fill <- map(by_conditions, ~split(., .$fill))
  names(by_fill) <- levels(event_files_filtered$conditions)
  gg_df <- unlist(by_fill, recursive = F)
  y_position_df <- map2(gg_df, mean_trap$mean_val,  ~dplyr::filter(.x, abs(x - .y) == min(abs(x - .y))))
  get_y <- map_dbl(y_position_df, `[[`, 'y')
  mean_trap$y  <- get_y
  
  
  # 
  # 
  # 
  
  g <- g +
    geom_point(data = mean_trap, aes(x = mean_val, y = y), size = 2)+
    geom_segment(data = mean_trap, aes(x = mean_val, y = y, xend = mean_val+0.05, yend = y+2))+
    geom_label(data = mean_trap, aes(x = mean_val+0.05, y = y+2, label = label))+
    # geom_col(data = mean_trap, aes(conditions, s2n, fill = stiffness))+
    coord_cartesian(c(0, 0.3))
  
  # geom_point(data = blue_data, aes(x = x, y = density), size = 2)+
  # geom_segment(data = blue_data, aes(x = x, y = density, xend = x+0.05, yend = density+2))+
  # geom_label(data = blue_data, aes(x = x+0.05, y = density+2, label = paste0("\u03bc = ", blue_mean) ))
  annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
  {
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
  }
  
  get_inset <- function(df){
    ggplot(df, aes(conditions, s2n, fill = stiffness))+
      geom_col(alpha = 0.5)+
      geom_text(aes(label = s2n), position = position_stack(vjust = 0.5), size = 5)+ 
      scale_fill_manual(values = c('blue', 'red'))+
      # scale_y_continuous(position = 'right')+
      coord_flip()+
      ggtitle('Signal-to-noise')+
      theme_nothing()+
      theme(legend.position = 'none',
            # axis.title.x  = element_text(size = 14, face = 'bold'),
            #axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 14, face = 'bold'))
    
  }
  
  insets <- mean_trap %>% 
    split(f = .$conditions) %>%
    purrr::map(~annotation_custom2(
      grob = ggplotGrob(get_inset(.)),
      data = data.frame(conditions=unique(.$conditions)),
      ymin = 50, ymax=60, xmin=0.22, xmax=0.28)
    )
  
  g <- g+insets

}