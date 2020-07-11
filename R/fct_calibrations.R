#' Equipartition method for calibrating trap stiffness
#'
#' @param data A vector of trap data
#' 
#' @export
#'
equipartition <- function(data){
  
  kb <-  4.10 #boltzman constant
  
  signal_var <- var(data) #get variance of data trace
  
  ep <- signal_var/kb  #equation from you, me, and dupuis
  
  return(ep)
  
}


#' Performs Step Calibration of the Laser Trap
#'
#' @param vector A numeric vector of trap data
#' @param step The known distance of each step in nanometers (i.e. how far did you move the stage)
#' 
#' @return A list containing 1) The mV difference between before/after step
#'    2) the mv2nm_conversion of values from #1
#'    3) a ggplot of the data trace
#'    
#' @import ggplot
#' 
#' @export
#' @example 
#' #Read in raw trap data from step cal something like
#' #data <- readr::read_tsv('Step.txt', col_names = c('bead', 'trap'))
#' todays_step_cal <- (data$bead, 50)

step_cal <- function(data, step){
  
  find_changepoint <- changepoint::cpt.mean(data)
  
  change_point <- changepoint::cpts(find_changepoint)
  
  indices <- list(c(1, change_point - 1000),
                  c(change_point + 1000, length(data)))
  
  meanr <- vector()
  for(i in seq_along(indices)){
    
    meanr[i] <- mean(data[indices[[i]][1]:indices[[i]][2]])
    
  }
  
  find_diff <- diff(meanr)
  
  conversion <- round(abs(step/find_diff), 2)
  
  #plot
  
  xdat1 <- seq(indices[[1]][1], indices[[1]][2], by = 0.5)
  
  xdat2 <- seq(indices[[2]][1], indices[[2]][2], by = 0.5)
  
  
  
  plots <- ggplot()+
    geom_line(data= as.data.frame(data), aes(x = 1:length(data), y = data))+
    geom_line(aes(x = xdat1, y = meanr[[1]]), color = "hotpink", size = 1.5)+
    geom_line(aes(x = xdat2, y = meanr[[2]]), color = "hotpink", size = 1.5)+
    xlab("Datapoints")+
    ylab("mV")+
    ggtitle(paste0(abs(round(conversion, 2)), "nm/mV"))+
    theme_bw()
  
  
  results <- list(mv_diff = find_diff,
                  mv2nm_conversion = conversion,
                  plot = plots)
  
  return(results)
  
}
