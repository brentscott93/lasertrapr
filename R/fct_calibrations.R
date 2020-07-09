#' Equipartition method for calibrating trap stiffness
#'
#' @param data a vector of trap data
#' 
#' @export
#'
equipartition <- function(data){
  
  kb <-  4.10 #boltzman constant
  
  signal_var <- var(data) #get variance of data trace
  
  ep <- signal_var/kb  #equation from you, me, and dupuis
  
  return(ep)
  
}
