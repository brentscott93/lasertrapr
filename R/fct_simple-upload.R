#' Simple Upload of laser trap data
#'
#' @param input_data uploaded data to app from fileInput
#' @param project user selected project folder
#' @param conditions user selected conditions
#' @param date user selected date
#'
#' @return nothing. saves data to lasertrapr folder
#'
simple_upload <- function(input_data,
                          project,
                          conditions,
                          date,
                          ready_for_analysis,
                          nm2pn_val,
                          hz,
                          number_of_channels,
                          downsample_by){
  withProgress(message = 'Initializing Data', value = 0, {
  #read in uploaded datq
   # browser()

  input_data <- dplyr::arrange(input_data, name)
  data_traces <- purrr::map(input_data$datapath,  data.table::fread)
  
  for(r in seq_along(data_traces)){
    incProgress(amount = 1/length(data_traces), detail = paste0("Obs", r))

    o <- data.table(project = project$name,
                    conditions = conditions$name,
                    date = date$name,
                    hz = hz,
                    processor = NA,
                    include = NA,
                    mv2nm = NA,
                    nm2pn = NA,
                    analyzer = NA,
                    report = 'not run',
                    review = NA,
                    channels = number_of_channels,
                    original_filename = input_data$name[[r]]
                    )

    t <- data.table(project = project$name,
                    conditions = conditions$name,
                    date = date$name, 
                    raw_bead =  data_traces[[r]][[1]],
                    trap_position = 0
                    )


          if(downsample_by != 1){
            downsample_by_vec <- seq(1, nrow(t), by = downsample_by)
             t <- t[downsample_by_vec,]
          }
    #t %<>% dplyr::mutate(processed_bead = data_traces[[r]][[1]]) untested change
    if(ready_for_analysis){
      t[, processed_bead := data_traces[[r]][[1]] ]
      
      o[, `:=`( processor = "user",
               include = TRUE,
               mv2nm = 1,
               nm2pn = nm2pn_val )
        ]
    }
    
    if(r < 10){
      #dev
      #dir.create(paste0(date, "/obs-0", r))
      dir.create(paste0(date$path, "/obs-0", r))
      t <- t |> dplyr::mutate(obs =  paste0("obs-0", r)) |> dplyr::select(project, conditions, date, obs, everything())
      data.table::fwrite(t, file = file.path(date$path, paste0("obs-0", r), "trap-data.csv"), sep = ",")
      o <- o |> dplyr::mutate(obs =  paste0("obs-0", r)) |> dplyr::select(project, conditions, date, obs, everything())
      data.table::fwrite(o, file = file.path(date$path, paste0("obs-0", r), "options.csv"), sep = ",")
    } else {
      #dev
      #dir.create(paste0(date,"/obs-", r))
      dir.create(paste0(date$path,"/obs-", r))
      t <- t |> dplyr::mutate(obs =  paste0("obs-", r)) |> dplyr::select(project, conditions, date, obs, everything())
      data.table::fwrite(t, file = file.path(date$path, paste0("obs-", r), "trap-data.csv"), sep = ",")
      o <- o |> dplyr::mutate(obs =  paste0("obs-", r)) |> dplyr::select(project, conditions, date, obs, everything())
      data.table::fwrite(o, file = file.path(date$path, paste0("obs-", r), "options.csv"), sep = ",")
    }
  }
setProgress(1, detail = "Done")
  })

}




##' @title Read in data with calibration info in the header 
##' @param input_data a dataframe returned by shinyFiles
##' @param h user inputted header lines list. values are line/row numbers
##' @param project 
##' @param conditions 
##' @param date 
##' @param number_of_channels 
##' @return nothing. saves data to lasertrapr folder
upload_data_cal_in_header <- function(input_data,
                                      h = h,
                                      project,
                                      conditions,
                                      date,
                                      number_of_channels,
                                      downsample_by){
    withProgress(message = 'Initializing Data', value = 0, {
       ## browser()
        for(r in seq_along(input_data$datapath)){
            
            shiny::incProgress(1/nrow(input_data))
            
            header_data <- fread(input_data$datapath[[r]],
                                 nrows = h$header_size,
                                 header = FALSE)

             o <- data.frame(project = project$name,
                             conditions = conditions$name,
                             date = date$name,
                             obs = NA,
                             hz = header_data[h$hz]$V2,
                             processor = NA,
                             include = NA,
                             mv2nm = header_data[h$nm_v1]$V2,
                             nm2pn = header_data[h$pn_nm1]$V2,
                             analyzer = NA,
                             report = 'not run',
                             review = NA,
                             channels = 1,
                             lab = "unknown",
                             original_filename = input_data$name[[r]])

            
             trap_data <- fread(input_data$datapath[[r]],
                                skip = h$header_size)
            
              t <- data.frame(project = project$name,
                              conditions = conditions$name,
                              date = date$name,
                              obs = NA,
                              raw_bead_1 = unname(unlist(trap_data[,h$trap1_col, with=FALSE])))

              if(number_of_channels == 2){
                  t$raw_bead_2 <- unname(unlist(trap_data[, h$trap2_col, with = FALSE]))
                  o$mv2nm2 <- header_data[h$nm_v2]$V2
                  o$nm2pn2 <- header_data[h$pn_nm2]$V2
                  o$channels <- 2
                  o$feedback_motor_bead <- header_data[h$feedback_motor_bead]$V2
              }


          if(nrow(header_data)>=30){
            if(header_data$V1[30] == "FB-Mbead Trap"){
                o$lab <- "greenberg"
                o$feedback_motor_bead <- header_data$V2[30]
                t$aod_position = trap_data$AODpos
                t$feedback_filter_error = trap_data$`FB-FiltErr`
                }
            }
         
            if(r < 10){
               obs <- paste0("obs-0", r)
             } else {
                obs <- paste0("obs-", r)
             }

            dir.create(file.path(date$path, obs))
            
            t$obs <- obs

          ## browser()
          if(downsample_by != 1){
            downsample_by_vec <- seq(1, nrow(t), by = downsample_by)
             t <- t[downsample_by_vec,]
          }

            data.table::fwrite(t, file = file.path(date$path, obs, "trap-data.csv"), sep = ",")

            o$obs <- obs
            data.table::fwrite(o, file = file.path(date$path, obs, "options.csv"), sep = ",")
            data.table::fwrite(header_data, file.path(date$path, obs, "header.csv"), sep = ",")
                
 
              }
    }
    )
}
