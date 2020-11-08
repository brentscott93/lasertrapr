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