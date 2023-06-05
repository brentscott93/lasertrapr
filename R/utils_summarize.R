#' Combine and save all "measured-events.csv" files
#' @param project a lasertrapr project folder
#' @param save_to_summary a logical. If TRUE will save to project/summary folder
#' @return df or nothing. saves a .csv to the project/summary folder named "{date}_{project}_all-measured-events.csv"
#' @export
#' @examples rbind_measured_events(project = "project_my-project")
rbind_measured_events <- function(project, save_to_summary = FALSE){
 #project = "project_step-time-validation-simulations"
 project_path <- file.path("~", "lasertrapr", project)

  #get options.csv to see what data should be included
  options_paths <-
    list.files(project_path,
               pattern = "options.csv",
               recursive = TRUE,
               full.names = TRUE)

  options_data <- data.table::rbindlist(lapply(options_paths, data.table::fread, nrows = 1), fill = TRUE)
  filtered_options <- options_data[include == TRUE & report == "success" & review == TRUE]

  filtered_options[, measured_events_path := file.path("~",
                                                       "lasertrapr",
                                                       project,
                                                       conditions,
                                                       date,
                                                       obs,
                                                       "measured-events.csv")]

  measured_events <- data.table::rbindlist(lapply(filtered_options$measured_events_path, data.table::fread), fill = TRUE)
  measured_events_filtered  <- measured_events[keep == TRUE & event_user_excluded == FALSE]

  summary_folder <- file.path(project_path, "summary")

  if(save_to_summary){
    if(!dir.exists(summary_folder)){
      dir.create(summary_folder)
    }
    data.table::fwrite(measured_events_filtered,
                       file.path(summary_folder, paste(Sys.Date(),
                                                       project,
                                                       "all-measured-events.csv",
                                                        sep = "_")
                               )
                     )

  }
  return(measured_events_filtered)
}

#' Quick Summary
#' @param all_measured_events a df made from rbind_measured_events
#' @export
summarize_trap <- function(all_measured_events, by){

all_measured_events[,
                    .(time_on_avg = mean(time_on_ms, na.rm = TRUE),
                      time_on_se = plotrix::std.error(time_on_ms, na.rm = TRUE),
                      time_on_sd = sd(time_on_ms, na.rm = TRUE),
                      time_on_median = median(time_on_ms, na.rm = TRUE),
                      time_off_avg = mean(time_off_ms, na.rm = TRUE),
                      time_off_se = plotrix::std.error(time_off_ms, na.rm = TRUE),
                      time_off_sd = sd(time_off_ms, na.rm = TRUE),
                      displacement_avg = mean(displacement_nm, na.rm = TRUE),
                      displacement_se = plotrix::std.error(displacement_nm, na.rm = TRUE),
                      displacement_sd = sd(displacement_nm, na.rm = TRUE),
                      force_avg = mean(force, na.rm = TRUE),
                      force_se = plotrix::std.error(force, na.rm = TRUE),
                      force_sd = sd(force, na.rm = TRUE),
                      trap_stiffness = mean(trap_stiffness, na.rm = T),
                      myo_stiffness = mean(myo_stiffness, na.rm = T),
                      num_events = .N),
                    by = by]
}


#' Calculate Total Time of Data Collection for each condition
#' @param project character. a lasertrapr project name
#' @param is_shiny logical
#' @export
total_trap_time <- function(project, is_shiny = FALSE){

  project_path <- file.path("~", "lasertrapr", project)

  #get options.csv to see what data should be included
  options_paths <-
    list.files(project_path,
               pattern = "options.csv",
               recursive = TRUE,
               full.names = TRUE)

  options_data <- data.table::rbindlist(lapply(options_paths, data.table::fread), fill = TRUE)
  filtered_options <- options_data[include == TRUE & report == "success" & review == TRUE]

  filtered_options[, trap_data_path := file.path("~",
                                                "lasertrapr",
                                                project,
                                                conditions,
                                                date,
                                                obs,
                                               "trap-data.csv")]

  get_time <- vector("list")
  for(r in 1:nrow(filtered_options)){
    if(is_shiny){incProgress(0.0025)}
    print(paste("Reading", r, "/", nrow(filtered_options)))
    td <- data.table::fread(filtered_options$trap_data_path[[r]])
    dp <- nrow(td)
    hz <- filtered_options$hz[[r]]

    get_time[[r]] <- data.frame(conditions = filtered_options$conditions[[r]],
                                minutes = round((dp/hz)/60, 2))

  }

    get_time <- data.table::rbindlist(get_time, fill = TRUE)

    sum_time <- get_time[,
                         .(minutes = sum(minutes)),
                         by = conditions]
    return(sum_time)
}

#' Quick Summary
#' @param df a df with conditions column to split
#' @export
split_conditions_column <- function(df, var_names, sep){

  # define a helper function
  split_conditions <- function(x, n, sep){
    strsplit(x, split = sep, fixed = TRUE)[[1]][[n]]
  }

  for(n in seq_along(var_names)){
    col_name <- var_names[[n]]
    df[[col_name]]  <- sapply(as.character(df$conditions),
                                           split_conditions,
                                           n = n,
                                           sep = sep)
  }

  return(df)
}
