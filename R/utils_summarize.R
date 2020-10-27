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