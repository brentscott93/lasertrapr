#' Converts milliseconds to datapoints
#'
#' @param x a millisecond value
#' @noRd
#' 
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
#' @return a 2 column tibble
#' @noRd

list_files <- function(...){
  tibble(name = list.files(full.names = FALSE, ...),
         path = list.files(full.names = TRUE, ...))
}

#' List folders into tibble
#'
#' @param ... arguments to dir function
#' @noRd

list_dir <- function(path){
  tibble(name = list.dirs(path = path, full.names = FALSE, recursive = F),
         path = list.dirs(path = path, full.names = TRUE, recursive = F))
}

#' Extract date time from trap data files
#'
#' @param x A laser trap raw data file name
#' @noRd

str_trap <- function(x){
  
  substring <- str_sub(x, c(6, 11, 14, 17, 20, 23), c(9, 12, 15, 18, 21, 24))

  final_string <- as.numeric(str_c(substring, collapse = ""))
  
  
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
#' 
#' @export

add_labels <- function(x, events, ...){
  for(event in 1:length(events)){
    x <- dygraphs::dyEvent(x, events[[event]], paste0("F", event), ...)
  }
  x
}

add_labels_hmm <- function(x, events, ...){
  for(event in 1:length(events$peak_nm_index)){
    x <- dygraphs::dyEvent(x, events$peak_nm_index[[event]], 
                           paste0(round(events$time_on_ms[[event]], digits = 0), 'ms, ',
                                  round(events$displacement_nm[[event]], digits = 1),
                                  ' nm {', event, '}'), ...)
  }
  x
}

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
#'
#' @export
#'

add_shades <- function(x, periods, ...){
  for(p in 1:nrow(periods)){
    x <- dygraphs::dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], color = periods$color[[p]], ...)
  }
  x
}

#' initialize/create a lasertrapr tibble
#' @description It will be easier to create the initial lasertrapr nested tibble with a function so if/when it changes
#'     it only has to be updated here and not the 3 places where this can happen (split_obs, move_obs, and trim_obs)
#' @param project A character vector - The name of a project
#' @param conditions A character vector - The name of conditions
#' @param date A character vector - The date
#' @param obs A character vector - The folder/obs
#' @param grouped A df/tibble with the trap data
#'
#' @noRd
#' 
create_lasertrapr_tibble <- function(project, conditions, date, obs, raw_bead, trap_position){
  tibble::tibble(project = project,
                 conditions = conditions,
                 date = date, 
                 obs = obs,
                 raw_bead = raw_bead,
                 trap_position = trap_position,
                 processed_bead = NA,
                 processor = NA,
                 include = NA,
                 mv2nm = NA, 
                 nm2pn = NA,
                 analyzer = NA,
                 report = 'not run',
                 review = NA)
}

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
    df %<>% tidyr::nest(data = c(raw_bead, processed_bead)) %>% 
    dplyr::select(!data)
  } else {
    df %<>% tidyr::nest(data = c(raw_bead, processed_bead, hm_overlay)) %>% 
      dplyr::select(!data)
  }
}

t1 <- fread("~/lasertrapr/project_myoV-phosphate/myoV-WT_pH-7.0_0mM-Pi/2020-06-25/obs-14/trap-data.csv")
t2 <- fread("~/lasertrapr/project_myoV-phosphate/myoV-WT_pH-7.0_0mM-Pi/2020-06-25/obs-14/measured-events.csv")

#' Plot trap data with ID'd events
#'
#' @param obs_path 
#' @param time_period_dp 
#' @param color 
#' @param axes 
#'
#' @return
#' @export
#'
#' @examples
plot_overlay <- function(obs_path, time_period_dp, color){
   # obs_path <- "~/lasertrapr/project_myoV-phosphate/myoV-WT_pH-7.0_0mM-Pi/2020-06-25/obs-14"
   # time_period_dp <- c(500/5000, 25000/5000)
   # color = "red"
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
   
   measured_events_path <-  list.files(obs_path,
                                  pattern = "measured-events.csv",
                                  full.names = TRUE)
   
  
   
   trap_data <- data.table::fread(trap_data_path, 
                                  select = "processed_bead")
   trap_data <- trap_data[time_period_dp[[1]]:time_period_dp[[2]],]
   
   trap_data %<>% mutate(real_time_index = time_period_dp[[1]]:time_period_dp[[2]],
                       new_time_index = 1:nrow(trap_data),
                       population = "baseline")
   
   measured_events <- data.table::fread(measured_events_path)
   measured_events <- dplyr::filter(measured_events, 
                                    start >= time_period_dp[[1]],
                                    stop <= time_period_dp[[2]])
   
   events <- list()
   for(i in 1:nrow(measured_events)){
      events[[i]] <- 
        trap_data %>% 
          dplyr::filter(real_time_index >= measured_events$start[[i]],
                        real_time_index <= measured_events$stop[[i]]) %>% 
          mutate(population = paste0("event", measured_events$index[[i]])) %>% 
        dplyr::select(new_time_index, 
                      real_time_index, 
                      processed_bead, 
                      population)
   }

events <- data.table::rbindlist(events)
  
all_data <- 
  trap_data %>% 
   dplyr::select(new_time_index, 
                 real_time_index, 
                 processed_bead, 
                 population) %>% 
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

  #  if(axes){
  #   par(mar=c(2, 2, 2, 2))
  #   plot(trap_data$processed_bead, type = "l", axes = FALSE, xlab = "", ylab = "")
  #   axis(2, seq(-100, 100, by = 20))
  #   axis(1)
  #   box(bty = "l", lwd = 1.1)
  # }  else {
  #   par(mar=c(0, 0, 0, 0))
  #   plot(trap_data$processed_bead, type = "l", axes = FALSE, xlab = "", ylab = "")
  # }
  # 
  # for(i in 1:nrow(measured_events)){
  #  start <- measured_events$cp_event_start_dp[[i]]
  #  stop <- measured_events$cp_event_stop_dp[[i]]
  #  event_data <-  trap_data[start:stop,]
  #  lines(start:stop, event_data$processed_bead, col = "red", lwd = 1.25)
  # }
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
  if(rlang::is_empty(x)) showNotification(...)
  req(!rlang::is_empty(x))
}

#' Defends shiny app if x != y
#' @noRd
defend_if_not_equal <- function(x, y, ...){
  if(x != y) showNotification(...)
  req(x == y)
}

#' Defends shiny app if x == y
#' @noRd
defend_if_equal <- function(x, y, ...){
  if(x == y) showNotification(...)
  req(x != y)
}

#' Defends shiny app if x == ""
#' @noRd
defend_if_blank <- function(x, ...){
  if(x == "") showNotification(...)
  req(x)
}

#' Defends shiny app if is.null(x)
#' @noRd
defend_if_null <- function(x, ...){
  if(is.null(x)) showNotification(...)
  req(!is.null(x))
}
