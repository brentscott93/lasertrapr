#' @noRd
ee_fread <- function(path, is_shiny = TRUE){
  if(is_shiny) incProgress(0.001)
  x <- data.table::fread(path) 
  x$path <- path
  return(x)
}

#' @noRd
prep_forward_ensemble_lm <- function(x, hz){
  x %>% 
    dplyr::filter(between(ensemble_index, 0, 10)) %>%
    mutate(time = ensemble_index/hz) 
}

#' @noRd
prep_forward_ensemble_exp <- function(x, hz){
  x %>% 
    dplyr::filter(ensemble_index >= 0) %>%
    mutate(time = ensemble_index/hz) 
}

#' @noRd
prep_backwards_ensemble_exp <- function(x, hz){
  x %>% 
    dplyr::filter(ensemble_index <=  0) %>%
    mutate(time = seq(0, by = 1/hz, along.with = ensemble_index))
}

