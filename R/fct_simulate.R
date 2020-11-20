#' Simulate Single Molecule Trap Data
#'
#' @param n an integer. The number of events. 
#' @param displacement a list. Containing mean and sd describing the displacement distribution (gaussian). 
#' @param time_on a list. Containing the rate, lower, and upper limits (in milliseconds) to the time on distribution (exponential). 
#' @param time_off a list. Containing the rate, lower, and upper limits (in milliseconds) to the time off distribution (exponential). 
#' @param khz an integer. Sampling frequency to simulate in kHz. 
#' @param baseline a list. Containing mean and sd describing the baseline noise. 
#' @param signal_to_noise an integer. Ratio of the signal to noise. 
#' @param pi_gates_stroke a list (or FALSE). If you want to simulating a delayed stroke include a list containing
#'                        rate, upper, and lower limits of an exponential distribution.
#' @param hitch a list (or FALSE). If you want to simulate the hitch include a list containing  
#'              rate, upper, and lower limits of an exponential distribution.
#'
#' @return a list containing a vector of simulated data and plots of the distributions from 10000 random draws. 
#' @export
#' @import truncdist ggplot2 cowplot
#' @examples simulate_single_molecule_trap_data(n = 500, 
#'                                              displacement = list(mean = 6, sd = 8), 
#'                                              time_on = list(rate = 0.004, lower = 15, upper = 100),
#'                                              time_off = list(rate = 0.002, lower = 15, upper = 5000),  
#'                                              khz = 5,
#'                                              baseline = list(mean = 0, sd = 8), 
#'                                              signal_to_noise = 2.5,
#'                                              hitch = list(rate = 0.004, lower = 15, upper = 1000)
#'                                              pi_gates_stroke = FALSE)
#' 
simulate_single_molecule_trap_data <- function(n, 
                                               displacement, 
                                               time_on, 
                                               time_off,
                                               khz,
                                               baseline, 
                                               signal_to_noise,
                                               pi_gates_stroke = FALSE,
                                               hitch = FALSE){
  results <- list()
  for(i in 1:n){
    #time off value in milliseconds
    #draw number from a truncated exponential distribution
    off <- round(
              rtrunc(1, 
                     spec = "exp", 
                     a = time_off$lower, 
                     b = time_off$upper, 
                     rate = time_off$rate), 
            0)
    #convert ms to datapoints in 5khz time
    off <- off*khz
    
    #on times
    on <- round(
            rtrunc(1, 
                   spec = "exp", 
                   a = time_on$lower, 
                   b = time_on$upper, 
                   rate = time_on$rate), 
            0)
     on <- on*khz
    
    
    step_size <- rnorm(1, mean = displacement$mean, sd = displacement$sd)
    
    base <- rnorm(off, mean = baseline$mean, sd = baseline$sd)
    
    if(is.list(pi_gates_stroke)){
      pause_time <- round(rtrunc(1, 
                                 spec = "exp", 
                                 a = pi_gates_stroke$lower, 
                                 b = pi_gates_stroke$upper,
                                 rate = pi_gates_stroke$rate), 
                          0)
      pause_time <- pause_time * khz
      pause <- rnorm(pause_time, baseline$mean, sd = displacement$sd)
      pause_df <- data.frame(data = pause,
                             key = "pause",
                             key_value = baseline$mean)
    } else {
      pause_df <- data.frame()
    }
    
    event_df  <- data.frame(data = rnorm(time_on, step_size, sd = sqrt((baseline$sd^2)/signal_to_noise)),
                            key = "event",
                            key_value = step_size)
    
    base_df <- data.frame(data = base, 
                          key = "baseline",
                          key_value = baseline$mean)
    
    if(is.list(hitch)){
      hitch_time <- round(rtrunc(1, 
                                 spec = "exp", 
                                 a = hitch$lower, 
                                 b = hitch$upper,
                                 rate = hitch$rate), 
                          0)
      hitch_time <- hitch_time * khz
      the_hitch <- rnorm(hitch_time, (step_size + hitch$nm), sd = displacement$sd)
      hitch_df <- data.frame(data = the_hitch,
                             key = "hitch",
                             key_value = (step_size + hitch$nm))
    } else {
      hitch_df <- data.frame()
    }
    
    
    results[[i]] <- rbind(base_df, pause_df, event_df, hitch_df)
  }
  
  data <- rbind(do.call("rbind", results), rnorm(1000*khz, baseline$mean, baseline$sd))
  
  data$time <- 1:nrow(data)/(khz*1000)

  on_plot <- data.frame(data = rtrunc(100000, 
                                      spec = "exp", 
                                      a = time_on$lower, 
                                      b = time_on$upper, 
                                      rate = time_on$rate),
                        key = "Time On")
  
  off_plot <- data.frame(data = rtrunc(100000, 
                                       spec = "exp", 
                                       a = time_off$lower, 
                                       b = time_off$upper, 
                                       rate = time_off$rate), 
                        key = "Time Off")
  
  step_plot <- data.frame(data = rnorm(100000, mean = displacement$mean, sd = displacement$sd),
                          key = "Displacement")
  
  if(is.list(pi_gates_stroke)){
    pause_plot <- data.frame(data = rtrunc(100000, 
                                            spec = "exp", 
                                            a = pi_gates_stroke$lower, 
                                            b = pi_gates_stroke$upper,
                                            rate = pi_gates_stroke$rate),
                             key = "Powerstroke Delay")
  } else {
    pause_plot <- data.frame()
  }
  
  if(is.list(hitch)){
    hitch_plot <- data.frame(data = rtrunc(100000, 
                                           spec = "exp", 
                                           a = hitch$lower, 
                                           b = hitch$upper,
                                           rate = hitch$rate), 
                             key = "Hitch Time")
  } else {
    hitch_plot <- data.frame()
  }
      
  plot_data <- rbind(step_plot, on_plot, off_plot, pause_plot, hitch_plot)
  
  plot <-  ggplot(data = plot_data, aes(x = data))+
            geom_histogram(aes(fill = key), bins = 100, color = "black")+
            facet_wrap(~key, scales = "free_x")+
            ggtitle("Simulating 100,000 draws from user defined distributions")
            scale_y_continuous(expand = expansion(0, 1))+
            xlab("")+
            theme_linedraw()+
            theme(legend.position = "none")
  
  return(list(data = data, plot = plot))
  
}