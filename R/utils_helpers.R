#' Converts milliseconds to datapoints
#'
#' @param x a millisecond value
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