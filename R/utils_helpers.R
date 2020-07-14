#' Converts milliseconds to datapoints
#'
#' @param x a millisecond value
#' @noRd
#' 
ms_to_dp <- function(x){
  sec <- x/1000 #to seconds
  hz <- 1/5000 # datapoints per second
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

list_dir <- function(...){
  tibble(name = dir(..., full.names = FALSE),
         path = dir(..., full.names = TRUE))
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

add_labels_hmm <- function(x, events, peak_nm_index, ...){
  for(event in 1:length(peak_nm_index)){
    x <- dygraphs::dyEvent(x, peak_nm_index[[event]], paste(round(events$time_on_ms[[event]], digits = 0), 'ms,', round(events$displacement_nm[[event]], digits = 1), 'nm'), ...)
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
    x <- dygraphs::dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], ...)
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
create_lasertrapr_tibble <- function(project, conditions, date, obs, grouped){
  tibble::tibble(project = project ,
                 conditions = conditions,
                 date = date, 
                 obs = obs,
                 grouped = list(grouped),
                 include = NA,
                 processed = NA,
                 processed_how = NA,
                 mv2nm = NA, 
                 nm2pn = NA,
                 status = 'grouped',
                 analyzer = NA,
                 results = NA,
                 report = NA,
                 quality_control = NA)
}

#' Read in all trap-data.rds files in an obs folder
#' @param f_date A list_files/list_dir tibble from the user selected date, literally f$date
#' @param f_date_input The date selectInput, literally f$date_input
#' @noRd
get_status_table <- function(f_date, f_date_input){
  if(is.null(f_date_input)) showNotification('Whoops. You forgot to select a date folder.', type = 'error')
  req(!is.null(f_date_input))
  a <- attempt::attempt(f_date_input == '')
  if(f_date_input == '') showNotification('Whoops. You forgot to select a date folder.', type = 'error')
  req(f_date_input)

  all_trap_paths <- list_files(f_date$path, pattern = 'trap-data.rds', recursive = T)
  if(is_empty(all_trap_paths)) showNotification("No trap-data.rds files in date folder yet. Start by loading date with 'Initialize Data'",  type = 'error')
  req(!is_empty(all_trap_paths))
  purrr::map_df(all_trap_paths$path, readRDS) 
}