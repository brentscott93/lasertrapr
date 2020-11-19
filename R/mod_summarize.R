#' summarize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summarize_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             box(title = 'Input',
                 width = NULL,
                 h4('Selected Project'),
                 verbatimTextOutput(ns('current_project')),
                 uiOutput(ns('user_defaults')),
                 radioButtons(ns('save_report'),
                              label = 'Save Report?',
                              choices = c('No', 'HTML', 'PPT'),
                              inline = T),
                 actionButton(ns('go'),
                              'Summarize',
                              icon = icon('calculator'),
                              width = '100%')
                 )
             ),
      column(9,
        box(width = NULL,
          title = 'Summary Table',
          # The id lets us use input$tabset1 on the server to find the current tab
          id = ns("project_summary"),

                  DT::DTOutput(ns('table')) %>% shinycssloaders::withSpinner(type = 8, color = '#373B38'),

          )
        )
      ),
    fluidRow(
      column(12,
      tabBox(width = NULL,

          title = '',
          # The id lets us use input$tabset1 on the server to find the current tab
          id = ns("distributions"),
          tabPanel("Displacements", plotOutput(ns('step'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Force",  plotOutput(ns('force'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Time On",  plotOutput(ns('ton'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Time Off", plotOutput(ns('toff'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Ton Survival", plotOutput(ns('ton_survival'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Event Frequency", plotOutput(ns('ef'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Stiffness", plotOutput(ns('stiffness'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel("Correlations", plotOutput(ns('correlations'), height = '550px') %>% shinycssloaders::withSpinner(type = 8, color = '#373B38')),
          tabPanel('Ensemble Average', 'ensemble avaerage stuff here')
      )
    )
    )
  )
}

#' summarize Server Function
#'
#' @noRd
#' @import data.table ggpubr cowplot rstatix survival survminer patchwork
mod_summarize_server <- function(input, output, session, f){
  # ns <- session$ns
  # 
  # output$current_project <- renderText({
  #   validate(need(f$project$name, 'Select a project'))
  #   f$project$name
  # })
  # 
  # conditions <- reactive({
  #   req(f$project$path)
  #  list_dir(f$project$path) %>%
  #     dplyr::pull(name)
  # })
  # output$user_defaults <- renderUI({
  #   req(conditions())
  #   tagList(
  #     selectInput(ns('factor_order'),
  #                 label = 'Factor Order',
  #                 multiple = T,
  #                 choices = conditions()),
  #     purrr::map2(seq_along(conditions()),
  #                 RColorBrewer::brewer.pal(length(conditions()), 'Set1'),
  #                 ~div(style = 'display:inline-block', colourpicker::colourInput(ns(paste0('color', .x)),
  #                                            label = paste('Color', .x),
  #                                            value = .y)))
  #   )
  # })
  # 
  # 
  # data <- reactiveValues(summary = data.frame(x = letters),
  #                        save = 0)
  # 
  # trap <- eventReactive(input$go, {
  #   golem::print_dev('summary go clicked')
  #   withProgress(message = 'Summarizing Project', {
  #      summarize_trap_data(f)
  #   })
  # })
  # 
  # 
  # 
  #   data$events <- event_files_filtered
  #   data$summary <- summarize_trap  %>%
  #     mutate_if(is.numeric, round, digits = 2)
  #    plot_colors <- purrr::map_chr(paste0('color', seq_along(conditions())), ~input[[.x]])
  #   withProgress(message = 'Plotting Distributions', {
  #    #  browser()
  #     setProgress(0.25, detail = 'Displacements')
  #   golem::print_dev(  levels(event_files_filtered$conditions) )
  #   comparisons <-combn(levels(event_files_filtered$conditions), 2, simplify = FALSE)
  # 
  #     step_histo <- ggplot(data = event_files_filtered,
  #                     aes(x = displacement_nm,
  #                         fill = conditions))+
  #       geom_histogram(aes(y = stat(density)),
  #                      binwidth = 3,
  #                      color = "black",
  #                      size = 0.9)+
  #       facet_wrap(~conditions, ncol = 2)+
  #       xlab("Step Size (nm)")+
  #       scale_y_continuous(expand = c(0,0))+
  #       scale_x_continuous(breaks = seq(-100, 100, by = 10))+
  #       scale_fill_manual(values = plot_colors)+
  #       #scale_fill_brewer(palette = "Dark2")+
  #       theme_linedraw(base_size = 11)+
  #       theme(panel.grid = element_blank(),
  #             legend.position = 'none')
  # 
  #     step_aov <-  event_files_filtered %>%
  #       rstatix::anova_test(displacement_nm ~ conditions)
  # 
  #     # event_files_filtered %>%
  #     #  anova_test(displacement_nm ~ myo+pi+myo*pi)
  # 
  # 
  #     step_tukey <-  event_files_filtered %>%
  #       stats::aov(displacement_nm ~ conditions, data = .) %>%
  #       rstatix::tukey_hsd()
  # 
  #     step_is_sig <- which(step_tukey$p.adj < 0.05)
  # 
  #     step_table <- step_tukey %>%
  #       dplyr::select(group1, group2, p.adj) %>%
  #       dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #       ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL) %>%
  #       ggpubr::table_cell_bg(row = step_is_sig + 1, column = 3, fill = '#ffa9a6')
  # 
  #     step_tukey <- rstatix::add_xy_position(step_tukey, data = data$events, formula = displacement_nm ~conditions)
  # 
  # 
  #     step_violin <- ggpubr::ggviolin(event_files_filtered,
  #                                     x = "conditions",
  #                                     y = "displacement_nm",
  #                                     fill = "conditions",
  #                                     palette = plot_colors,
  #                                     add = "boxplot",
  #                                     xlab = '',
  #                                     ylab = 'Displacement (nm)',
  #                                     ggtheme = ggpubr::theme_pubr(base_size=11))+
  #       ggpubr::stat_pvalue_manual(step_tukey, bracket.nudge.y = seq(10, by = 4, length.out = nrow(step_tukey)))+
  #       scale_y_continuous(breaks = seq(-50, 1000, by = 15))+
  #       rotate_x_text(angle = 45)+
  #      # rotate()+
  #       #yscale('log10', .format = T)+
  #       labs(
  #         subtitle = rstatix::get_test_label(step_aov, detailed = TRUE),
  #         caption = rstatix::get_pwc_label(step_tukey)
  #       ) +
  #       theme(legend.position = 'none')
  # 
  # 
  # 
  #   #  step_top <- cowplot::plot_grid(step_violin, step_histo, align = "hv", axis = "bt", rel_widths = c(1,1))
  #     step_side <- cowplot::plot_grid(step_histo, step_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  #     step_top <-  cowplot::plot_grid(step_violin, step_side, ncol = 2, nrow = 1,  rel_heights = c(1, 1))
  #     # now add the title
  #     step_title <- cowplot::ggdraw() +
  #       cowplot::draw_label(
  #         "Displacement Distributions, ANOVA, & Tukey Table",
  #         fontface = 'bold',
  #         size = 18,
  #         x = 0.5
  #       )
  # 
  #     p$step <- cowplot::plot_grid(step_title, step_top, #step_table,
  #                                  ncol = 1, nrow =2,
  #                                  rel_heights = c(0.05, 1))
  # 
  #     #### ton ####
  #     setProgress(0.5, detail = 'Time On')
  #     ton_histo <- ggplot(data = event_files_filtered,
  #                         aes(x = time_on_ms,
  #                             fill = conditions))+
  #       geom_histogram(aes(y = stat(density)),
  #                      binwidth = 20,
  #                      color = "black",
  #                      size = 0.9)+
  #       facet_wrap(~conditions, ncol = 2)+
  #       xlab("Time on (ms)")+
  #       scale_y_continuous(expand = c(0,0))+
  #       #scale_x_continuous(breaks = seq(0, 6000, by = 100))+
  #       coord_cartesian(c(0, 500))+
  #       scale_fill_manual(values = plot_colors)+
  #       #scale_fill_brewer(palette = "Dark2")+
  #       theme_linedraw(base_size = 11)+
  #       theme(panel.grid = element_blank(),
  #             legend.position = "none")
  # 
  #     ton_kruskal <-  event_files_filtered %>%
  #       rstatix::kruskal_test(time_on_ms ~ conditions)
  # 
  #     ton_wilcox <-   event_files_filtered %>%
  #       rstatix::wilcox_test(time_on_ms ~ conditions,  p.adjust.method = "holm")
  # 
  #     ton_is_sig <- which(ton_wilcox$p.adj < 0.05)
  # 
  #     ton_table <- ton_wilcox %>%
  #       dplyr::select(group1, group2, p.adj) %>%
  #       dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #       ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL) %>%
  #       ggpubr::table_cell_bg(row = ton_is_sig + 1, column = 3, fill = '#ffa9a6')
  # 
  #     ton_wilcox %<>% rstatix::add_xy_position(x = "conditions", y.trans = log10)
  # 
  # 
  # 
  #     ton_violin <- ggpubr::ggviolin(event_files_filtered,
  #                            x = "conditions",
  #                            y = "time_on_ms",
  #                            fill = "conditions",
  #                            palette = plot_colors,
  #                            add = "boxplot",
  #                            xlab = '',
  #                            ylab = 'Time On (ms)',
  #                            ggtheme = ggpubr::theme_pubr(base_size=11))+
  #       ggpubr::stat_pvalue_manual(ton_wilcox, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(ton_wilcox)))+
  #       yscale('log10', .format = T)+
  #       rotate_x_text(angle=45)+
  #       labs(
  #         subtitle = rstatix::get_test_label(ton_kruskal, detailed = TRUE),
  #         caption = rstatix::get_pwc_label(ton_wilcox)
  #       ) +
  #       theme(legend.position = 'none')
  # 
  # 
  # 
  #     ton_side <- cowplot::plot_grid(ton_histo, ton_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  #     ton_top <-  cowplot::plot_grid(ton_violin, ton_side, ncol = 2, nrow = 1)
  # 
  #     # now add the title
  #     ton_title <- cowplot::ggdraw() +
  #       cowplot::draw_label(
  #         "Time On Distributions, Kruskal-Wallis, & Pairwise Wilcox Table",
  #         fontface = 'bold',
  #         size = 18,
  #         x = 0.5
  #       )
  # 
  #    p$ton <- cowplot::plot_grid(ton_title, ton_top, #ton_table,
  #               ncol = 1, nrow =3,
  #               rel_heights = c(0.05, 1))
  # 
  # 
  #    setProgress(0.75, detail = 'Time Off')
  #    toff_histo <- ggplot(data = event_files_filtered,
  #                         aes(x = time_off_ms,
  #                             fill = conditions))+
  #      geom_histogram(aes(y = stat(density)),
  #                     binwidth = 100,
  #                     color = "black",
  #                     size = 0.9)+
  #      facet_wrap(~conditions, ncol=2)+
  #      xlab("Time Off (ms)")+
  #      scale_y_continuous(expand = c(0,0))+
  #      #scale_x_continuous(breaks = seq(0, 20000, by = 500))+
  #      coord_cartesian(c(0, 2000))+
  #      scale_fill_manual(values = plot_colors)+
  #      #scale_fill_brewer(palette = "Dark2")+
  #      theme_linedraw(base_size = 11)+
  #      theme(panel.grid = element_blank(),
  #            legend.position = "none")
  # 
  # 
  #    toff_kruskal <-  event_files_filtered %>%
  #      rstatix::kruskal_test(time_off_ms ~ conditions)
  # 
  #    toff_wilcox <-   event_files_filtered %>%
  #      wilcox_test(time_off_ms ~ conditions,  p.adjust.method = "holm")
  # 
  #    toff_is_sig <- which(toff_wilcox$p.adj < 0.05)
  # 
  #    toff_table <- toff_wilcox %>%
  #      dplyr::select(group1, group2, p.adj) %>%
  #      dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #      ggpubr::ggtexttable (theme = ttheme(base_style = 'light', base_size = 9), rows = NULL) %>%
  #      ggpubr::table_cell_bg(row = toff_is_sig + 1, column = 3, fill = '#ffa9a6')
  # 
  #    toff_wilcox %<>% add_xy_position(x = "conditions", y.trans = log10)
  # 
  # 
  #    toff_violin <- ggviolin(event_files_filtered,
  #                            x = "conditions",
  #                            y = "time_off_ms",
  #                            fill = "conditions",
  #                            palette = plot_colors,
  #                            add = "boxplot",
  #                            xlab = '',
  #                            ylab = 'Time Off (ms)',
  #                            ggtheme = theme_pubr(base_size=11))+
  #      stat_pvalue_manual(toff_wilcox, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(toff_wilcox)))+
  #      yscale('log10', .format = T)+
  #      rotate_x_text(angle=45)+
  #      labs(
  #        subtitle = get_test_label(toff_kruskal, detailed = TRUE),
  #        caption = get_pwc_label(toff_wilcox)
  #      ) +
  #      theme(legend.position = 'none')
  # 
  #    #ton_violin
  #    # ggarrange(ton_histo, ton_violin, time_on_test_table,
  # 
  #    #           ncol = 3, nrow = 1,
  #    #           widths = c(1, 1, 0.5))
  # 
  # 
  #    toff_side <- cowplot::plot_grid(toff_histo, toff_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  #    toff_top <-  cowplot::plot_grid(toff_violin, toff_side, ncol = 2, nrow = 1)
  # 
  #    # now add the title
  #    toff_title <- ggdraw() +
  #      draw_label(
  #        "Time Off Distributions, Kruskal-Wallis, & Pairwise Wilcox Table",
  #        fontface = 'bold',
  #        size = 18,
  #        x = 0.5
  #      )
  # 
  #   p$toff <-  plot_grid(toff_title, toff_top, #toff_table,
  #              ncol = 1, nrow =3,
  #              rel_heights = c(0.05, 1))
  # 
  # 
  #   #### FORCE DISTRIBUTIONS ####
  #   setProgress(0.95, detail = 'Force')
  #   force_histo <- ggplot(data = event_files_filtered,
  #                         aes(x = force,
  #                             fill = conditions))+
  #     geom_histogram(aes(y = stat(density)),
  #                    binwidth = 0.1,
  #                    color = "black",
  #                    size = 0.9)+
  #     facet_wrap(~conditions, ncol=2)+
  #     xlab("Force (pN)")+
  #     scale_y_continuous(expand = c(0,0))+
  #     #scale_x_continuous(breaks = seq(0, 20000, by = 500))+
  #     scale_fill_manual(values = plot_colors)+
  #     #scale_fill_brewer(palette = "Dark2")+
  #     theme_linedraw(base_size = 11)+
  #     theme(panel.grid = element_blank(),
  #           legend.position = "none")
  # 
  # 
  #   force_aov <-  event_files_filtered %>%
  #     anova_test(force ~ conditions)
  # 
  #   force_tukey <-  event_files_filtered %>%
  #     aov(force~conditions, data = .) %>%
  #     rstatix::tukey_hsd()
  # 
  #   force_is_sig <- which(force_tukey$p.adj < 0.05)
  # 
  #   force_table <- force_tukey %>%
  #     dplyr::select(group1, group2, p.adj) %>%
  #     dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #     ggpubr::ggtexttable (theme = ggpubr::ttheme(base_style = 'light', base_size = 9), rows = NULL) %>%
  #     ggpubr::table_cell_bg(row = force_is_sig + 1, column = 3, fill = '#ffa9a6')
  # 
  #   force_tukey <- add_xy_position(force_tukey, data = event_files_filtered, formula = force ~ conditions)
  # 
  # 
  #   force_violin <- ggviolin(event_files_filtered,
  #                            x = "conditions",
  #                            y = "force",
  #                            fill = "conditions",
  #                            palette = plot_colors,
  #                            add = "boxplot",
  #                            xlab = '',
  #                            ylab = 'Time Off (ms)',
  #                            ggtheme = theme_pubr(base_size=11))+
  #     stat_pvalue_manual(force_tukey, bracket.nudge.y = seq(0.5, by = 0.3, length.out = nrow(toff_wilcox)))+
  #     scale_y_continuous(breaks = seq(-3, 5, by = 1))+
  #     rotate_x_text(angle=45)+
  #     #yscale('log10', .format = T)+
  #     labs(
  #       subtitle = get_test_label(force_aov, detailed = TRUE),
  #       caption = get_pwc_label(force_tukey)
  #     ) +
  #     theme(legend.position = 'none')
  # 
  # 
  # 
  # 
  #   force_side <- cowplot::plot_grid(force_histo, force_table, ncol = 1, nrow = 2,  rel_heights = c(0.75,0.25))
  #   force_top <-  cowplot::plot_grid(force_violin, force_side, ncol = 2, nrow = 1)
  # 
  #   # now add the title
  #   force_title <- cowplot::ggdraw() +
  #     cowplot::draw_label(
  #       "Force Distributions, ANOVA, & Tukey Table",
  #       fontface = 'bold',
  #       size = 18,
  #       x = 0.5
  #     )
  # 
  #   p$force <- cowplot::plot_grid(force_title, force_top, #force_table,
  #                 ncol = 1, nrow =3,
  #                 rel_heights = c(0.05, 1))
  #   setProgress(1, detail = 'Done')
  #   })
  # 
  #   withProgress(message = 'Survival Analysis', {
  #    # browser()
  #    # df <- event_files_filtered
  # 
  #     fit <- survfit(Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  # 
  #     ggsurvival <- ggsurvplot(fit,
  #                              data = event_files_filtered,
  #                              ggtheme = theme_linedraw(base_size = 12), # Change ggplot2 theme
  #                              palette = plot_colors,
  #                              xlim = c(0, 1000),
  #                              break.time.by = 150,
  #                              fun = 'event',
  #                              # pval = T
  #     )
  # 
  #     all_surv <- ggsurvival$plot +
  #       xlab('Time (ms)')+
  #       xlab("")+
  #       theme(panel.grid = element_blank(),
  #             legend.title = element_blank(),
  #       )
  # 
  #     facet_surv <- ggsurvival$plot +
  #       facet_wrap(~conditions)+
  #       xlab('Time (ms)')+
  #       theme(panel.grid = element_blank(),
  #             legend.title = element_blank(),
  #             legend.position = 'none')
  # 
  #     p$ton_survival <- all_surv/facet_surv
  # 
  #     data$ton_log_rank_test <- survdiff(Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  #     data$ton_cox <- coxph(Surv(time_on_ms) ~ conditions, data = event_files_filtered)
  #   })
  # 
  #   #### EVENT FREQ ####
  #   withProgress(message = 'Event Frequency', {
  #     #browser()
  #     ef_paths <- str_replace(event_file_paths, "measured-events", "event-frequency")
  # 
  #     ef_read <- function(path){
  #       vroom::vroom(path, delim = ",") %>%
  #         dplyr::mutate(path = path)
  #     }
  #     ef <- map_df(ef_paths, ef_read) %>%
  #       separate(path, c('root', 'lt', 'project', 'conditions', 'date', 'obs', 'file'), sep = '/') #%>%
  #      # mutate(conditions_to_sep = conditions) %>%
  #      # separate(conditions_to_sep, c("myo", 'ph', 'pi'), sep = "_") %>%
  #       #mutate(conditions2 = new_names[conditions])
  # 
  # 
  # 
  #     ef$conditions <- factor(ef$conditions, levels = input$factor_order)
  # 
  # 
  #     ef_kruskal <- ef %>% kruskal_test(freq_start ~ conditions)
  # 
  #     ef_wilcox <-  ef %>%
  #       wilcox_test(freq_start ~ conditions, p.adjust.method = "holm")
  #     ef_is_sig <- which(ef_wilcox$p.adj < 0.05)
  # 
  #     ef_table <- ef_wilcox %>%
  #       dplyr::select(group1, group2, p.adj) %>%
  #       dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #       ggtexttable (theme = ttheme(base_style = 'light', base_size = 10), rows = NULL) %>%
  #       table_cell_bg(row = ef_is_sig + 1, column = 3, fill = '#ffa9a6')
  # 
  #     ef_wilcox %<>% add_xy_position(x = "conditions")
  # 
  #     ef_plot <- ggplot(data = ef, aes(conditions, freq_start))+
  #       geom_jitter(aes(color = conditions), size = 2, alpha = 0.8)+
  #       stat_pvalue_manual(ef_wilcox) +
  #       stat_summary(
  #         fun.data = mean_sdl,  fun.args = list(mult = 1),
  #         geom = "pointrange", color = "black"
  #       )+
  #       scale_color_manual(values = plot_colors)+
  #       scale_y_continuous(breaks = 0:15)+
  #       xlab('')+
  #       ylab('Events/sec')+
  #       labs(
  #         title = 'Event Frequency',
  #         subtitle = get_test_label(ef_kruskal, detailed = TRUE),
  #         caption = get_pwc_label(ef_wilcox)
  #       )+
  #       theme_cowplot(font_size = 11)+
  #       theme(legend.position = 'none')#+
  # 
  # 
  # 
  #    p$ef <- plot_grid(ef_plot, ef_table, ncol = 2, nrow = 1, rel_widths = c(2, 1))
  # 
  #    #no zero
  #    # ef_no0 <- dplyr::filter(ef, freq_start != 0 )
  #    # ef_kruskal_no_zero <- ef_no0 %>%
  #    #   kruskal_test(freq_start ~ conditions)
  #    #
  #    # ef_wilcox_no_zero <-  ef_no0 %>%
  #    #   wilcox_test(freq_start ~ conditions, p.adjust.method = "holm")
  #    #
  #    # ef_is_sig_no_zero <- which(ef_wilcox_no_zero$p.adj < 0.05)
  #    #
  #    # ef_table_no_zero <- ef_wilcox_no_zero %>%
  #    #   dplyr::select(group1, group2, p.adj) %>%
  #    #   dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  #    #   ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light'), rows = NULL) %>%
  #    #   table_cell_bg(row = ef_is_sig_no_zero + 1, column = 3, fill = '#ffa9a6')
  #    #
  #    # ef_wilcox_no_zero %<>% add_xy_position(x = "conditions")
  #    #
  #    # ef_plot_no_zero <- ggplot(data = ef_no0, aes(conditions, freq_start))+
  #    #   geom_jitter(aes(color = conditions), size = 2, alpha = 0.8)+
  #    #   stat_pvalue_manual(ef_wilcox_no_zero) +
  #    #   stat_summary(
  #    #     fun.data = mean_sdl,  fun.args = list(mult = 1),
  #    #     geom = "pointrange", color = "black"
  #    #   )+
  #    #   scale_color_manual(values = plot_colors)+
  #    #   scale_y_continuous(breaks = 0:15)+
  #    #   xlab('')+
  #    #   ylab('Events/sec')+
  #    #   labs(
  #    #     title = 'Event Frequency (zero counts eliminated)',
  #    #     subtitle = get_test_label(ef_kruskal_no_zero, detailed = TRUE),
  #    #     caption = get_pwc_label(ef_wilcox_no_zero)
  #    #   )+
  #    #   theme_cowplot(font_size = 12)+
  #    #   theme(legend.position = 'none')#+
  #    #
  #    # p$ef_filter <- plot_grid(ef_plot_no_zero, ef_table_no_zero, ncol = 2, nrow = 1, rel_widths = c(1, 0.4))
  #   })
  #   data$save <- data$save + 1
  # })
  # 
  # observeEvent(data$save, ignoreInit = T,  {
  #   withProgress(message = 'Saving Report', {
  #     temp_report <- file.path(tempdir(), "project-summary.Rmd")
  # 
  #     report_file <- system.file("rmd", "project-summary-flex.Rmd", package = "lasertrapr")
  # 
  #     file.copy(report_file, temp_report, overwrite = TRUE)
  #     # Set up parameters to pass to Rmd document
  #     params <- list(p = p,
  #                    data = data)
  # 
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(temp_report, output_dir = f$project$path,
  #                       params = params,
  #                       envir = new.env(parent = globalenv()) )
  #   })
  # })
  # output$table <- DT::renderDT({
  #   validate(need('conditions' %in% colnames(trap()$summary), 'Select completed project, choose options, and click summarize'))
  #   trap()$summary$conditions <- factor(summarize_trap$conditions,
  #                                       levels = input$factor_order)
  #   trap()$summary %>%
  #    dplyr::select( 'Conditions' = conditions,
  #                   "Step Size (nm)" = displacement_avg,
  #                   "SE Step" = displacement_se,
  #                   "Force (pN)" = force_avg,
  #                   "SE Force" = force_se,
  #                   "Time On (ms)" = time_on_avg,
  #                   "SE Ton" = time_on_se,
  #                   'Median Time-on' = time_on_median,
  #                   "Time Off (ms)" = time_off_avg,
  #                   "SE Toff" = time_off_se,
  #                   "No. Events" = num_events,
  #                   "Minutes Collected" = minutes ) %>%
  #   dplyr::arrange(Conditions) %>%
  #   DT::datatable(
  #             extensions = 'FixedColumns',
  #             options = list(
  #               dom = 't',
  #               scrollX = TRUE,
  #               fixedColumns = list(leftColumns = 2)
  #             )
  #   )
  # })
  # 
  # 
  # p <- reactiveValues()
  # 
  # output$step <- renderPlot({
  #   req(p$step)
  #   p$step
  # })
  # 
  # output$ton <- renderPlot({
  #   req(p$ton)
  #   p$ton
  # })
  # 
  # output$toff <- renderPlot({
  #   req(p$toff)
  #   p$toff
  # })
  # 
  # output$force <- renderPlot({
  #   req(p$force)
  #   p$force
  # })
  # 
  # output$ton_survival <- renderPlot({
  #   req(p$ton_survival)
  #   p$ton_survival
  # })
  # 
  # output$ef <- renderPlot({
  #   req(p$ef)
  #   p$ef
  # })
}

## To be copied in the UI
# mod_summarize_ui("summarize")

## To be copied in the server
# callModule(mod_summarize_server, "summarize")

