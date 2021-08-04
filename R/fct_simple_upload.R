#' Simple Upload of laser trap data
#'
#' @param input_data uploaded data to app from fileInput
#' @param project user selected project folder
#' @param conditions user selected conditions
#' @param date user selected date
#'
#' @return nothing. saves data to lasertrapr folder
#'
simple_upload <- function(input_data, project, conditions, date, ready_for_analysis, nm2pn){
  withProgress(message = 'Initializing Data', value = 0, {
  #read in uploaded datq
   # browser()
  input_data <- dplyr::arrange(input_data, name)
  data_traces <- purrr::map(input_data$datapath,  data.table::fread)
  
  for(r in seq_along(data_traces)){
    incProgress(amount = 1/length(data_traces), detail = paste0("Obs", r))
  
    t <- create_lasertrapr_tibble( project = project$name,
                                   conditions = conditions$name,
                                   date = date$name, 
                                   obs = paste0('obs-0', r),
                                   raw_bead =  data_traces[[r]][[1]],
                                   trap_position = 0)
    t %<>% dplyr::mutate(processed_bead = data_traces[[r]][[1]])
    if(ready_for_analysis){
      t %<>% dplyr::mutate(processor = "user",
                           include = TRUE, 
                           mv2nm = 1, 
                           nm2pn = nm2pn)
    }
    if(r < 10){
      #dev
      #dir.create(paste0(date, "/obs-0", r))
      dir.create(paste0(date$path, "/obs-0", r))
      data.table::fwrite(t, file = file.path(date$path, paste0("obs-0", r), "trap-data.csv"), sep = ",")
    } else {
      #dev
      #dir.create(paste0(date,"/obs-", r))
      dir.create(paste0(date$path,"/obs-", r))
      data.table::fwrite(t, file = file.path(date$path, paste0("obs-", r), "trap-data.csv"), sep = ",")
    }
  }
  
setProgress(1, detail = "Done")
  })

}

