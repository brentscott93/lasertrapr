#' Simulate Single Molecule Trap Data
#'
#' @param n an integer. The number of events. 
#' @param displacement a list. Containing mean and sd describing the displacement distribution (gaussian). 
#' @param pi_release a list. Containing the rate, lower, and upper limits (in milliseconds) to the time on distribution (exponential). 
#' @param adp_release a list. Containing the rate, lower, and upper limits (in milliseconds) to the time off distribution (exponential). 
#' @param hz an integer. Sampling frequency to simulate in Hz. 
#' @param baseline a list. Containing mean and sd describing the baseline noise. 
#' @param signal_to_noise an integer. Ratio of the signal to noise. 
#' 

#' @return a list containing a vector of simulated data and plots of the distributions from 10000 random draws. 
#' @export
#' @import truncdist ggplot2 cowplot
#' @examples simulate_single_molecule_trap_data(n = 100, 
#'                                              signal_to_noise = 2.5,
#'                                              hz = 5000,
#'                                              baseline = list(mean = 0, sd = 8), 
#'                                              time_off = list(rate = 50, lower = 15, upper = 5000),
#'                                              displacement = list(mean = 6, sd = 8), 
#'                                              pi_release = list(rate = 200, lower = 0, upper = 1500, occurs = 'after'),
#'                                              adp_release = list(rate = 20, lower = 0, upper = 1000, hitch = 2),
#'                                              atp_binding = list(rate = 1000, lower = 0, upper = 10000, hitch = 2) )
#' 
simulate_single_molecule_trap_data <- function(n, 
                                               hz,
                                               signal_to_noise,
                                               baseline, 
                                               displacement, 
                                               pi_release, 
                                               adp_release,
                                               atp_binding, 
                                               time_off){

  results <- list()
  for(i in 1:n){
    print(i)
    #time off value in milliseconds
    #draw number from a truncated exponential distribution
    off <- round(
              rtrunc(1, 
                     spec = "exp", 
                     a = time_off$lower, 
                     b = time_off$upper, 
                     rate = 1/time_off$rate
                     ), 
            0)
    #convert seconds to datapoints in hz time
    off_time <- round((1/off)*hz, 0)
    
    baseline_df <- data.frame(data = rnorm(off_time, mean = baseline$mean, sd = baseline$sd), 
                              key = "baseline",
                              key_value = baseline$mean, 
                              state = 1)
    
    step_size <- round(rnorm(1, mean = displacement$mean, sd = displacement$sd), 2)
    
    if(pi_release != "uncoupled"){
      #on times
      pi <- round(
              rtrunc(1, 
                     spec = "exp", 
                     a = pi_release$lower, 
                     b = pi_release$upper, 
                     rate = 1/pi_release$rate
                     ), 
              0)
      
      pi_release_time <- round((1/pi)*hz, 0)
      
  
      if(pi_release$occurs == "before"){
        pi_release_df <- data.frame(data = rnorm(pi_release_time, mean = baseline$mean, sd = sqrt((baseline$sd^2)/signal_to_noise)),
                                    key = 'pi_release',
                                    key_value = baseline$mean, 
                                    state = 2)
      } else if(pi_release$occurs == "after"){
        pi_release_df <- data.frame(data = rnorm(pi_release_time, mean = step_size, sd = sqrt((baseline$sd^2)/signal_to_noise)),
                                    key = 'pi_release',
                                    key_value = step_size, 
                                    state = 2)
      }
    }
    
    if(is.null(adp_release$set_time)){
      #on times
      adp <- round(
        rtrunc(1, 
               spec = "exp", 
               a = adp_release$lower, 
               b = adp_release$upper, 
               rate = 1/adp_release$rate
        ), 
        0)
      
      
      
      adp_release_time <- round((1/adp)*hz, 0)
      
    } else {
      adp_release_time <- round((1/adp_release$set_time)*hz, 0)
      
    }
      

    
    adp_release_df <- data.frame(data = rnorm(adp_release_time, mean = step_size, sd = sqrt((baseline$sd^2)/signal_to_noise)),
                                 key = 'adp_release',
                                 key_value = step_size, 
                                 state = 2) 
   
    if(is.null(atp_binding$set_time)){
    #on times
      atp <- round(
        rtrunc(1, 
               spec = "exp", 
               a = atp_binding$lower, 
               b = atp_binding$upper, 
               rate = 1/atp_binding$rate
        ), 
        0)
      
      atp_binding_time <- round((1/atp)*hz, 0)
    } else {
      
     atp_binding_time <- round((1/atp_binding$set_time)*hz, 0)
      
    }
      
      atp_binding_df <- data.frame(data = rnorm(atp_binding_time, mean = (step_size + adp_release$hitch), sd = sqrt((baseline$sd^2)/signal_to_noise)),
                                   key = 'atp_binding',
                                   key_value = step_size + adp_release$hitch, 
                                   state = 2) 

      
      

    if(pi_release == "uncoupled"){
      results[[i]] <- rbind(baseline_df, adp_release_df, atp_binding_df)
    } else {
      results[[i]] <- rbind(baseline_df, pi_release_df, adp_release_df, atp_binding_df)
    }
  }
  
  baseline_df_end <- data.frame(data = rnorm(1*hz, mean = baseline$mean, sd = baseline$sd), 
                            key = "baseline",
                            key_value = baseline$mean, 
                            state = 1)
  
  data <- rbind(do.call("rbind", results), baseline_df_end)
  
  data$time <- 1:nrow(data)/hz

  return(data)
}
