#' @noRd
ee_fread <- function(path){
  data.table::fread(path) %>% 
    dplyr::mutate(path = path)
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