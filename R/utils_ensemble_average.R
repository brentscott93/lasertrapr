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
    mutate(time = sort(seq(0, by = -1/hz, along.with = ensemble_index)))
}

#' xDP-release, xTP-binding self-starter (fct) for drm
#'
#' @return
#' @export
#'
#' @examples
drc_forward_ensemble_average_fit <- function(exp2 = TRUE){
  
  if(exp2){
    # binding_rates <- function(molar_conc, k_adp, k_atp){
    #   (k_adp + (k_atp*molar_conc)) / (k_atp*molar_conc*k_adp)
    # }
    
    ensemble_average_2exp <- function(time_x, d1, d2, k0, k1){
      (d1*(1 - exp(-k0 * time_x))) + (d2*(1 - exp(-k1 * time_x)))
    }
    
  
    # fct <- function(x, parm) {
    #   binding_rates(molar_conc = x, k_adp = parm[,1], k_atp = parm[,2])
    # }
    fct <- function(x, parm) {
      ensemble_average_2exp(time_x = x, d1 = parm[,1], d2 = parm[,2], k0 = parm[,3], k1 = parm[,4])
    }
    
    ssfct <- function(data){
      x <- data[, 1]
      y <- data[, 2]
      
      d1 <- mean(x[5:15])
      d2 <- mean(tail(y)) - d1
      
      k0 <- 
      k1 <- 
        
        
      start <- c(k_adp, k_atp)
      return(start)
    }
  
  
    # ssfct <- function(data){
    #   k_adp <- 500
    #   k_atp <- 2*10^6
    #   
    #   start <- c(k_adp, k_atp)
    #   return(start)
    # }
    names <- c("k_adp", "k_atp")
    text <- "xDP-release, xTP-binding"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
    class(returnList) <- "drcMean"
    invisible(returnList)
    
  } else {
  ######################################################################################3  
    binding_rates <- function(molar_conc, k_adp, k_atp){
      (k_adp + (k_atp*molar_conc)) / (k_atp*molar_conc*k_adp)
    }
    
    
    fct <- function(x, parm) {
      binding_rates(molar_conc = x, k_adp = parm[,1], k_atp = parm[,2])
    }
    
    ssfct <- function(data){
      k_adp <- 500
      k_atp <- 2*10^6
      
      start <- c(k_adp, k_atp)
      return(start)
    }
    names <- c("k_adp", "k_atp")
    text <- "xDP-release, xTP-binding"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
    class(returnList) <- "drcMean"
    invisible(returnList)
    
  }
}