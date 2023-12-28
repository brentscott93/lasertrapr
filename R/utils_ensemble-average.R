#' @noRd
ee_fread <- function(path, is_shiny = TRUE){
  if(is_shiny) incProgress(0.001)
  x <- data.table::fread(path) 
  x$path <- path
  return(x)
}

#' @noRd
prep_forward_ensemble_lm <- function(x, hz){
  x |>
    dplyr::filter(between(ensemble_index, 0, 10)) |>
    dplyr::mutate(time = ensemble_index/hz)
}

#' @noRd
prep_forward_ensemble_exp <- function(x, max_l, hz){
  x[ensemble_index >= 0 & ensemble_index <= max_l,
    .(ensemble_index,
      avg,
      sd,
      se,
      ## n,
      time = ensemble_index/hz)]
}

#' @noRd
fit_forward_ee_1exp <- function(ee_data, start){
  minpack.lm::nlsLM(avg ~ d1 + (d2*(1 - exp(-k1 * time))),
                    data = ee_data,
                    start = start,
                    control = minpack.lm::nls.lm.control(maxiter = 1000)
                    )
}

#' @noRd
fit_forward_ee_2exp <- function(ee_data){
  ## browser()
  start_list <- list(d1 = 5, d2 = 2, k0 = 1000, k1 = 100)
  fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k0 * time))) + (d2*(1 - exp(-k1 * time))),
                               data = ee_data,
                               start = start_list,
                               control = minpack.lm::nls.lm.control(maxiter = 1000)
                               ),
             silent = TRUE)

  if(class(fit)=="try-error"){
    start_list <- list(d1 = 5, d2 = 2, k0 = 1000, k1 = 1)
    fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k0 * time))) + (d2*(1 - exp(-k1 * time))),
                                 data = ee_data,
                                 start = start_list,
                                 control = minpack.lm::nls.lm.control(maxiter = 1000)
                                 ),
               silent = TRUE)
  }

  if(class(fit)=="try-error"){
    counter <- 1
    grid_search <- expand.grid(c(5, 10, 15), c(1, 5, 10), c(1000, 100, 5000, 1), c(100, 1, 50, 1000))
    while(counter <= 100){
      samp_row <- sample(1:nrow(grid_search), 1)
      start_list <- grid_search[samp_row,]
      starter <- list(d1 = start_list[, 1], d2 = start_list[, 2], k0 = start_list[, 3], k1 = start_list[, 4])

      fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k0 * time))) + (d2*(1 - exp(-k1 * time))),
                                   data = ee_data,
                                   start = starter,
                                   control = minpack.lm::nls.lm.control(maxiter = 1000)
                                   ),
                 silent = TRUE)
      if(class(fit)=="try-error"){
        counter <- counter + 1
      } else {
        counter <- 101
      }
    }
  }

  return(fit)
}



#' @noRd
fit_forward_ee_3exp <- function(ee_data){
  ## browser()
  start_list <- list(d1 = 1, d2 = 2, d3 = 3, k1 = 1000, k2 = 10, k3 = 70)
  fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k1 * time))) + (d2*(1 - exp(-k2 * time))) + (d3*(1 - exp(-k3 * time))),
                               data = ee_data,
                               start = start_list,
                               control = minpack.lm::nls.lm.control(maxiter = 1000)
                               ),
             silent = TRUE)

  if(class(fit)=="try-error"){
   start_list <- list(d1 = 1, d2 = 5, d3 = 2, k1 = 100, k2 = 100, k3 = 7)
  fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k1 * time))) + (d2*(1 - exp(-k2 * time))) + (d3*(1 - exp(-k3 * time))),
                                 data = ee_data,
                                 start = start_list,
                                 control = minpack.lm::nls.lm.control(maxiter = 1000)
                                 ),
               silent = TRUE)
  }

  if(class(fit)=="try-error"){
    counter <- 1
    grid_search <- expand.grid(c(5, 10, 15), c(1, 5, 10), c(1, 5, 10),
                               c(1000, 100, 5000, 1),
                               c(100, 1, 50, 1000),
                               c(1, 100, 1000, 50))
    while(counter <= 100){
      samp_row <- sample(1:nrow(grid_search), 1)
      start_list <- grid_search[samp_row,]
      starter <- list(d1 = start_list[, 1],
                      d2 = start_list[, 2],
                      d3 = start_list[, 3],
                      k1 = start_list[, 4],
                      k2 = start_list[, 5],
                      k3 = start_list[, 6])

  fit <- try(minpack.lm::nlsLM(avg ~ (d1*(1 - exp(-k1 * time))) + (d2*(1 - exp(-k2 * time))) + (d3*(1 - exp(-k3 * time))),
                                   data = ee_data,
                                   start = starter,
                                   control = minpack.lm::nls.lm.control(maxiter = 1000)
                                   ),
                 silent = TRUE)
      if(class(fit)=="try-error"){
        counter <- counter + 1
      } else {
        counter <- 101
      }
    }
  }

  return(fit)
}

#' @noRd
prep_backwards_ensemble_exp <- function(x, max_l, hz){
  x[ensemble_index <= 0 & ensemble_index >= -max_l,
    .(ensemble_index,
      avg,
      sd,
      se,
      ## n,
      time = sort(seq(0, by = -1/hz, along.with = ensemble_index)))]
}

#' @noRd
prep_backwards_baseline_shift <- function(x, hz){
  x[ensemble_index > 0,
    .(ensemble_index,
      avg,
      sd,
      se,
      ## n,
      time = seq(1/hz, by = 1/hz, along.with = ensemble_index))]
}
#' @noRd
fit_backwards_ee_1exp <- function(ee_data, start){
  minpack.lm::nlsLM(avg ~ d1+(d2*exp(time*k2)),
                    data = ee_data,
                    start = start)
}


#' @noRd
predict_ee <- function(nls_fit, hz, forward = TRUE){
  predict_y <- predict(nls_fit)
  if(forward){
    x <- seq(0, by = 1/hz, along.with = predict_y)
  } else {
    x <- sort(seq(0, by = -1/hz, along.with = predict_y))
 }
  data.frame(time = x,
             predict_y = predict_y)
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
