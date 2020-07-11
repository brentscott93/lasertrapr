#' Equipartition method for calibrating trap stiffness
#'
#' @param data A vector of trap data
#' 
#' @export
#' @examples
#' #Read in trap data something like:
#' #data <- readr::read_tsv('Equi.txt', col_names = c('bead', 'trap'))
#' todays_equi_cal <- equipartition(data$bead)
#'
equipartition <- function(data){
  
  if(!is.vector(data)) stop("'data' is not a vector. Did you forget to subset your dataframe with '$'?")
  
  kb <-  4.10 #boltzman constant
  
  signal_var <- var(data) #get variance of data trace
  
  ep <- signal_var/kb  #equation from you, me, and dupuis
  
  return(ep)
  
}


#' Performs Step Calibration of the Laser Trap
#'
#' @description This function applies \code{changepoint::cpt.mean()} to the supplied data (should be a Step.txt file) 
#'     and calculates the mean of the of signal before and after the step. This will work only if the calibration was performed with one
#'     step per file. Ideally the trace would look like this: ~~~¬____
#'     
#'     
#' @param data A numeric vector of trap data
#' @param step The known distance of each step in nanometers (i.e. how far did you move the stage)
#' 
#' @return A list containing the mV difference between before/after step, mv2nm_conversion, and a plot
#'    
#' 
#' @export
#' @examples 
#' #Read in raw trap data from step cal something like:
#' #data <- readr::read_tsv('Step.txt', col_names = c('bead', 'trap'))
#' todays_step_cal <- step_cal(data$bead, 50)

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
