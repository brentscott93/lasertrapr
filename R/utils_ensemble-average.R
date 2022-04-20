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
  x[ensemble_index >= 0, .(ensemble_index, 
                           avg,
                           sd, 
                           se, 
                           n,
                           time = ensemble_index/hz)]
}

#' @noRd
prep_backwards_ensemble_exp <- function(x, hz){
  x %>% 
    dplyr::filter(ensemble_index <=  0) %>%
    mutate(time = sort(seq(0, by = -1/hz, along.with = ensemble_index)))
}




#' Ensemble average self starter
#' @noRd
drc_single_neg_exp <- function(){
  
  single_neg_exp <- function(time, a, c){
    a * (1 - exp (- c * time))
  }
  
  
  fct <- function(x, parm) {
    single_neg_exp(time = x, a = parm[,1], c = parm[,2])
  }
  
  ssfct <- function(data){a
    x <- data[, 1]
    y <- data[, 2]
    
    a <- mean(tail(y))
    c <- 500
    
    
    start <- c(a, c)
    return(start)
  }
  names <- c("Plateua (a)", "Rate (c)")
  text <- "Negative Single Exponential"
  
  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
  
}

#' Ensemble average self starter
#' @noRd
drc_forward_ee_1exp <- function(){
  
  forward_ee_1exp <- function(time, d1, d2, k1){
    d1 + d2 *(1 - exp (- k1 * time))
  }
  
  
  fct <- function(x, parm) {
    forward_ee_1exp(time = x, d1 = parm[,1], d2 = parm[,2], k1 = parm[,3])
  }
  
  ssfct <- function(data){
    x <- data[, 1]
    y <- data[, 2]
    
    d1 <- mean(y[1:5])
    d2 <- mean(tail(y)) - d1
    k1 <- 500    
    
    start <- c(d1, d2, k1)
    return(start)
  }
  names <- c("d1", "d2", "k1")
  text <- "Forward Ensemble Average 1-exp"
  
  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
  
}



#' Ensemble average self starter
#' @noRd
drc_forward_ee_2exp <- function(){
 
    ensemble_average_2exp <- function(time_x, d1, d2, k0, k1){
      (d1*(1 - exp(-k0 * time_x))) + (d2*(1 - exp(-k1 * time_x)))
    }
    

    fct <- function(x, parm) {
      ensemble_average_2exp(time_x = x, d1 = parm[,1], d2 = parm[,2], k0 = parm[,3], k1 = parm[,4])
    }
    
    ssfct <- function(data){
      x <- data[, 1]
      y <- data[, 2]
      df <- data.frame(x = x[1:100],
                       y = y[1:100])
      d1 <- mean(y[5:15])
      d2 <- mean(tail(y)) - d1
      
      estimate_k0 <- drm(y~x, data = df, fct = drc_single_neg_exp())
      k0 <- estimate_k0$coefficients[["Rate (c):(Intercept)"]]
      estimate_k1 <- drm(y~x, data = df, fct = drc_forward_ee_1exp())
      k1 <- estimate_k1$coefficients[["k1:(Intercept)"]]
        
        
      start <- c(d1, d2, k0, k1)
      return(start)
    }
  

    names <- c("d1", "d2", "k0", "k1")
    text <- "Forward Ensemble Average 2-exp"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
    class(returnList) <- "drcMean"
    invisible(returnList)
 
}
