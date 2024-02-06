#' Converts milliseconds to datapoints
#'
#' @param x a millisecond value
#' @noRd
ms_to_dp <- function(x, hz = 5000){
  sec <- x/1000 #to seconds
  hz <- 1/hz # datapoints per second
  dp <- sec/hz
  return(dp)
}

#' Lists files into a tibble
#'
#' @param ... arguments to list.files function
#'
#' @return a 2 column df
#' @noRd
list_files <- function(...){
  data.frame(name = list.files(full.names = FALSE, ...),
             path = list.files(full.names = TRUE, ...))
}

#' List folders into tibble
#'
#' @param ... arguments to dir function
#' @noRd
list_dir <- function(path){
  data.frame(name = list.dirs(path = path, full.names = FALSE, recursive = F),
             path = list.dirs(path = path, full.names = TRUE, recursive = F))
}

#' Extract date time from trap data files
#'
#' @param x A laser trap raw data file name
#' @return special function for debold lab
#' @noRd
str_trap <- function(x){
  
  substring <- stringr::str_sub(x, c(6, 11, 14, 17, 20, 23), c(9, 12, 15, 18, 21, 24))

  final_string <- as.numeric(stringr::str_c(substring, collapse = ""))
  
  
}

#' Round to any integer from https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816
#'
#' @param x a number to round
#' @param accuracy a number specifying what nearest digit or number to round to
#' @param f function to use to round (defaults to round)
#' 
#' @export
round_any <- function(x, accuracy, f=round){
  
  f(x/accuracy) * accuracy
  
}



#' Add many labels to a dygraph
#'
#' @param x a dygraph object (usually piped in)
#' @param events a vector x-axis markers
#' @param ... additional arguments to dyEvent
#' @noRd
add_labels <- function(x, events, ...){
  for(event in 1:length(events)){
    x <- dygraphs::dyEvent(x, events[[event]], paste0("F", event), ...)
  }
  x
}

#'@noRd
add_labels_hmm <- function(x, events, ...){
  for(event in 1:length(events$peak_nm_index)){
    x <- dygraphs::dyEvent(x, events$peak_nm_index[[event]], 
                           paste0(round(events$time_on_ms[[event]], digits = 0), 'ms, ',
                                  round(events$displacement_nm[[event]], digits = 1),
                                  ' nm {', events$id[[event]], '}'), ...)
  }
  x
}

#'@noRd
add_labels_mini <- function(x, events, hz, ...){
  for(event in 1:length(events$peak_nm_index)){
    x <- dygraphs::dyEvent(x, events$peak_nm_index[[event]]/hz, 
                           paste0(round(events$time_on_ms[[event]], digits = 0), 'ms, ',
                                  round(events$displacement_nm[[event]], digits = 1),
                                  ' nm {', event, '}'), ...)
  }
  x
}


#' Add many shaing areas to a dygraph
#'
#' @param x A dygraph object (usually piped in)
#' @param periods A df with start and stop pairs to mark the shaded areas
#' @param ... additional arguments to dyEvent
#' @noRd
add_shades <- function(x, periods, ...){
  for(p in 1:nrow(periods)){
    x <- dygraphs::dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], color = periods$color[[p]], ...)
  }
  x
}



# create_lasertrapr_tibble <- function(project, conditions, date, obs, raw_bead, trap_position){
#   tibble::tibble(project = project,
#                  conditions = conditions,
#                  date = date,
#                  obs = obs,
#                  raw_bead = raw_bead,
#                  trap_position = trap_position,
#                  processed_bead = NA,
#                  processor = NA,
#                  include = NA,
#                  mv2nm = NA,
#                  nm2pn = NA,
#                  analyzer = NA,
#                  report = 'not run',
#                  review = NA)
# }


#' Read in all trap-data.csv files in an obs folder
#' @param f_date A list_files/list_dir tibble from the user selected date, literally f$date
#' @param f_date_input The date selectInput, literally f$date_input
#' @noRd
get_info_table <- function(f_date, f_date_input){
  
  defend_if_null(f_date_input, ui = 'Whoops. You forgot to select a date folder.', type = 'error')
  defend_if_blank(f_date_input, ui = 'Whoops. You forgot to select a date folder.', type = 'error')

  all_trap_paths <- list_files(f_date$path, pattern = 'trap-data.csv', recursive = T)
  defend_if_empty(all_trap_paths, ui = "No trap-data.rds files in date folder yet. Start by loading date with 'Initialize Data'",  type = 'error')
 
  df <- data.table::fread(all_trap_paths$path, sep = ",")
  if('hm_overlay' %not_in% colnames(df)){
    df <- tidyr::nest(df, data = c(raw_bead, processed_bead)) %>%
    dplyr::select(!data)
  } else {
    df <- tidyr::nest(df, data = c(raw_bead, processed_bead, hm_overlay)) %>%
      dplyr::select(!data)
  }
}

#' Plot trap data with ID'd events
#'
#' @param obs_path character string of a file path to a lasertrapr observation folder
#' @param time_period_dp numeric vector of length 2 which denotes start/end of the trace to plot
#' @param color character string of a valid hex color
#' @export
#' @return a ggplot object
#' @import data.table ggplot2
plot_overlay <- function(obs_path, time_period_dp, color, bead_offset = 0){
   # obs_path <- "~/lasertrapr/project_myoV-phosphate/myoV-WT_pH-7.0_0mM-Pi/2020-06-25/obs-14"
   # time_period_dp <- c(500/5000, 25000/5000)
   # color = "red"
  ## browser()
  options_path <-  measured_events <-  list.files(obs_path,
                                                  pattern = "options.csv",
                                                  full.names = TRUE)
  
  options <- data.table::fread(options_path)
  
  hz <- options$hz
  
  time_period_dp <- c(round_any(time_period_dp[[1]], 1/hz, f = round),
                      round_any(time_period_dp[[2]], 1/hz, f = round))
  
  time_period_dp <- time_period_dp*hz
   
   nrows <- length(time_period_dp[[1]]:time_period_dp[[2]]) 
   if(time_period_dp[[1]] <= 1){
     nskip <- 0 
    } else {
     nskip <- time_period_dp[[1]] - 1
    }
  
   trap_data_path <- list.files(obs_path,
                           pattern = "trap-data.csv",
                           full.names = TRUE)

   if(options$channels == 1){
   trap_data <- data.table::fread(trap_data_path,
                                  select = "processed_bead")
  } else if(options$channels == 2 ){

   trap_data <- data.table::fread(trap_data_path,
                                  select = c("processed_bead_1", "processed_bead_2"))
  }


   trap_data <- trap_data[time_period_dp[[1]]:time_period_dp[[2]],]

  trap_data <- trap_data[, `:=`(real_time_index = time_period_dp[[1]]:time_period_dp[[2]],
                             new_time_index = .I,
                             population = "baseline")]
   
   measured_events_path <-  list.files(obs_path,
                                  pattern = "measured-events.csv",
                                  full.names = TRUE)

   measured_events <- data.table::fread(measured_events_path)
   if(options$analyzer != "covar"){
   measured_events <- measured_events[cp_event_start_dp >= time_period_dp[[1]] & cp_event_stop_dp <= time_period_dp[[2]] ]
   } else {

   measured_events <- measured_events[cp_event_start_dp_1 >= time_period_dp[[1]] & cp_event_stop_dp_1 <= time_period_dp[[2]] ]

   }

if(options$analyzer == "hm/cp"){
   
   events <- list()
   for(i in 1:nrow(measured_events)){
      events[[i]] <- 
        trap_data |>
          dplyr::filter(real_time_index >= measured_events$start[[i]],
                        real_time_index <= measured_events$stop[[i]]) |>
          dplyr::mutate(population = paste0("event", measured_events$index[[i]])) |>
        dplyr::select(new_time_index, 
                      real_time_index, 
                      processed_bead, 
                      population)
   }

events <- data.table::rbindlist(events)
  
all_data <- 
  trap_data |>
   dplyr::select(new_time_index, 
                 real_time_index, 
                 processed_bead, 
                 population) |>
  rbind(events)


ggplot(all_data, aes(new_time_index/hz, processed_bead, color = population))+
  geom_line()+
  scale_color_manual(values = c("black", rep(color, nrow(measured_events))))+
  xlab("")+
  scale_x_continuous(breaks = seq(1, 100, 1))+
  ylab("")+
  scale_y_continuous(breaks = seq(-100, 100, by = 20))+
  theme_minimal_grid()+
  theme(
    legend.position = "none",
    axis.text = element_blank()
  )

} else if(options$analyzer == "ifc"){

  if(options$feedback_motor_bead == 1){

    gg_ifc <-
      ggplot(trap_data, aes(x = new_time_index/hz))+
      geom_line(aes(y = processed_bead_1 + bead_offset[1]),
                color = alpha(color, 0.5))+
      geom_line(aes( y = processed_bead_2 + bead_offset[2]),
                color = color)

  } else {

    gg_ifc <-
      ggplot(trap_data, aes(x = new_time_index/hz))+
      geom_line(aes(y = processed_bead_1 + bead_offset[1]),
                color = color)+
      geom_line(aes(y = processed_bead_2 + bead_offset[2]),
                color = alpha(color, 0.5))
  }

  minmin <-
    min(
      c(
        min(trap_data$processed_bead_1+bead_offset[1]),
        min(trap_data$processed_bead_2+bead_offset[2])
      )
    )


  maxmax <-
    max(
      c(
        max(trap_data$processed_bead_1+bead_offset[1]),
        max(trap_data$processed_bead_2+bead_offset[2])
      )
    )

  gg_ifc+
    xlab("")+
    scale_x_continuous(breaks = seq(0, 100, 0.1))+
    ylab("")+
    scale_y_continuous(breaks = seq(minmin, maxmax, by = 100))+
    theme_minimal_grid()+
    theme(
      legend.position = "none",
      axis.text = element_blank()
    )

} else if(options$analyzer == "covar"){

   events <- list()
   for(i in 1:nrow(measured_events)){
      events[[i]] <-
        trap_data |>
          dplyr::filter(real_time_index >= measured_events$cp_event_start_dp_1[[i]],
                        real_time_index <= measured_events$cp_event_stop_dp_1[[i]]) |>
          dplyr::mutate(population = paste0("event", i)) |>
        dplyr::select(new_time_index,
                      real_time_index,
                      processed_bead_1,
                      population)
   }

events <- data.table::rbindlist(events)

all_data <-
  trap_data |>
   dplyr::select(new_time_index,
                 real_time_index,
                 processed_bead_1,
                 population) |>
  rbind(events)


ggplot(all_data, aes(new_time_index/hz, processed_bead_1, color = population))+
  geom_line()+
  scale_color_manual(values = c("black", rep(color, nrow(measured_events))))+
  xlab("")+
  scale_x_continuous(breaks = seq(1, 100, 1))+
  ylab("")+
  scale_y_continuous(breaks = seq(-100, 100, by = 20))+
  theme_minimal_grid()+
  theme(
    legend.position = "none",
    axis.text = element_blank()
  )

}
}


#' Plot trap data with ID'd events
#'
#' @noRd
#' @return a ggplot object
#' @import data.table ggplot2 cowplot
ggplot_mini_events <- function(trap_data,
                               measured_events,
                               seconds = c(1, 10),
                               color = "red",
                               hz = 5000,
                               nm_scale = 100){

  x_axis_seconds <- seq(seconds[1], seconds[2], by = 1/hz) * hz

  trap_data <- fread(trap_data)
  measured_events <- fread(measured_events)

  trap_data_subset <- trap_data[x_axis_seconds,]

  measured_events <- measured_events[event_start >= seconds[1]*hz & event_start <= seconds[2]*hz,]


  p <-
    ggplot()+
    geom_line(data = trap_data_subset,
              aes(x = 1:nrow(trap_data_subset),
                  y = rescaled_mini_data),
              linewidth = 0.1)

  for(i in 1:nrow(measured_events)){
    print(i)
    start <- measured_events$event_start[i] - (seconds[1]*hz)
    stop <- measured_events$event_stop[i] - (seconds[1] * hz)
    event <- trap_data_subset[start:stop,]
    df <- data.frame(x = start:stop,
                     y = event$rescaled_mini_data)
    p <- p+geom_line(data = df,
                     aes(x = x,
                         y = y),
                     color = color,
                     linewidth = 0.1)
  }
  p+
    draw_line(x = rep(-500, 2),
              y = c(0, nm_scale),
              linewidth = 1)+
    draw_line(x = c(0, hz),
              y = rep(min(trap_data_subset$rescaled_mini_data), 2),
              linewidth = 1)+
    annotate("text", label = paste0(nm_scale, " nm"), x = -1200, y = nm_scale / 2, angle = 90)+
    annotate("text", label = paste0("1 s"), x = hz/2, vjust = 1.25, y = min(trap_data_subset$rescaled_mini_data))+
    coord_cartesian(ylim = c(min(trap_data_subset$rescaled_mini_data)-15, NA))+
    theme_void()
}
  
    
    


##########################################################################################################################################


#' Stops reactivity in shiny app if expression is TRUE
#'
#' @param .if An expression that returns TRUE/FALSE (to be passed to an internal if statement)
#' @param is_shiny TRUE/FALSE indicating whether using function inside a shiny app. Defaults to TRUE. Set to FALSE to practice using functions at the console. 
#' @param ... Additional arguments passed to shinyShowNotification
#'
#' @return Nothing. Will either stop or allow shiny reactivity to continue
#' @export
#'
#' @examples 
#' # This will stop reactivity and showNotification in UI
#' defend_if(TRUE, message = 'Oops!', type = 'error')
#' 
#' # This will allow reactivity to continue showing nothing in UI
#' defend_if(TRUE, message = 'Oops!', type = 'error')
#' 
#' # Practice at consolse with is_shiny = F
#' 
#' defend_if(TRUE, is_shiny = F)
defend_if <- function(.if, is_shiny = T, ...){
  if(is_shiny){
    if(.if) shiny::showNotification(...)
    shiny::req(.if == FALSE)
  } else {
    if(.if){
      cli::cli_alert_danger(crayon::red("Defended! - reactivity would stop in Shiny")) 
    } else {
      cli::cli_alert_success(crayon::green("Success! - reactivity would continue in Shiny"))
    }
  }
}


#' Allow reactivity to continue in shiny if expression is TRUE
#'
#' @param .if An expression that returns TRUE/FALSE (to be passed to an internal if statement)
#' @param is_shiny A logical. Default is TRUE.
#' @param ... Additional arguments passed to shinyShowNotification
#'
#' @return Nothing. Will either stop or allow shiny reactivity to continue
#' @export
#'
#' @examples 
#' # This will stop reactivity and showNotification in UI
#' allow_if(FALSE, message = 'Oops!', type = 'error')
#' 
#' # This will allow reactivity to continue showing nothing in UI
#' allow_if(TRUE, message = 'Oops!', type = 'error')
#' 
#' # Practice at consolse with is_shiny = F
#' 
#' allow_if(TRUE, is_shiny = F)
allow_if <- function(.if, is_shiny = T, ...){
  if(is_shiny){
      if(.if == FALSE) shiny::showNotification(...)
      shiny::req(.if)
  } else {
      if(.if == FALSE){
       cli::cli_alert_danger(crayon::red("Defended! - reactivity would stop in Shiny")) 
      } else {
       cli::cli_alert_success(crayon::green("Success! - reactivity would continue in Shiny"))
    }
  }
}

#' Defends shiny app if object is_empty(x)
#' @noRd
defend_if_empty <- function(x, ...){
  if(rlang::is_empty(x)) shiny::showNotification(...)
  req(!rlang::is_empty(x))
}

#' Defends shiny app if x != y
#' @noRd
defend_if_not_equal <- function(x, y, ...){
  if(x != y) shiny::showNotification(...)
  req(x == y)
}

#' Defends shiny app if x == y
#' @noRd
defend_if_equal <- function(x, y, ...){
  if(x == y) shiny::showNotification(...)
  req(x != y)
}

#' Defends shiny app if x == ""
#' @noRd
defend_if_blank <- function(x, ...){
  if(x == "") shiny::showNotification(...)
  req(x)
}

#' Defends shiny app if is.null(x)
#' @noRd
defend_if_null <- function(x, ...){
  if(is.null(x)) shiny::showNotification(...)
  req(!is.null(x))
}
