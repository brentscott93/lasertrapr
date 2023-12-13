#' Combine and save all "measured-events.csv" files
#' @param project a lasertrapr project folder
#' @param save_to_summary a logical. If TRUE will save to project/summary folder
#' @return df or nothing. saves a .csv to the project/summary folder named "{date}_{project}_all-measured-events.csv"
#' @export
#' @import data.table
#' @examples rbind_measured_events(project = "project_my-project")
rbind_measured_events <- function(project, save_to_summary = FALSE, n_channels = 1){
 #project = "project_step-time-validation-simulations"
 project_path <- file.path(path.expand("~"), "lasertrapr", project)

  #get options.csv to see what data should be included
  options_paths <-
    list.files(project_path,
               pattern = "options.csv",
               recursive = TRUE,
               full.names = TRUE)

  options_data <- data.table::rbindlist(lapply(options_paths, data.table::fread, nrows = 1), fill = TRUE)
  filtered_options <- options_data[include == TRUE & report == "success" & review == TRUE]

  is_unique <- length(unique(filtered_options$analyzer))==1
   if(!is_unique){
        stop("Data are not all analyzed with the same analyzer.")
      }

  analyzer <- unique(filtered_options$analyzer)

  if(analyzer == "hm/cp" || analyzer == "covar"){
  filtered_options[, measured_events_path := file.path(path.expand("~"),
                                                       "lasertrapr",
                                                       project,
                                                       conditions,
                                                       date,
                                                       obs,
                                                       "measured-events.csv")]
  } else if(analyzer == "mini"){
  filtered_options[, measured_events_path := file.path(path.expand("~"),
                                                       "lasertrapr",
                                                       project,
                                                       conditions,
                                                       date,
                                                       obs,
                                                       "mini-measured-events.csv")]
  } else if(analyzer == "ifc"){

  filtered_options[, measured_events_path := file.path(path.expand("~"),
                                                       "lasertrapr",
                                                       project,
                                                       conditions,
                                                       date,
                                                       obs,
                                                       "ifc-measured-events.csv")]
  }

  measured_events <- data.table::rbindlist(lapply(filtered_options$measured_events_path, data.table::fread), fill = TRUE)

  if(analyzer != "mini"){
    if(n_channels == 2){
      measured_events[, keep_add := keep_1+keep_2]
      measured_events[, keep := ifelse(keep_add == 2, TRUE, FALSE)]
    }
    measured_events_filtered  <- measured_events[keep == TRUE & event_user_excluded == FALSE]
  } else {
    measured_events_filtered <- measured_events
  }

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
#' @import data.table
#' @export
summarize_trap <- function(all_measured_events, by, n_channels = 1){
## browser()
  if(n_channels == 1){
    all_measured_events[,
                        .(time_on_avg = mean(time_on_ms, na.rm = TRUE),
                          time_on_se = sd(time_on_ms, na.rm = TRUE)/sqrt(.N),
                          time_on_sd = sd(time_on_ms, na.rm = TRUE),
                          time_on_median = median(time_on_ms, na.rm = TRUE),
                          time_off_avg = mean(time_off_ms, na.rm = TRUE),
                          time_off_se = sd(time_off_ms, na.rm = TRUE)/sqrt(.N),
                          time_off_sd = sd(time_off_ms, na.rm = TRUE),
                          displacement_avg = mean(displacement_nm, na.rm = TRUE),
                          displacement_se = sd(displacement_nm, na.rm = TRUE)/sqrt(.N),
                          displacement_sd = sd(displacement_nm, na.rm = TRUE),
                          force_avg = mean(force, na.rm = TRUE),
                          force_se = sd(force, na.rm = TRUE)/sqrt(.N),
                          force_sd = sd(force, na.rm = TRUE),
                          ## trap_stiffness = mean(trap_stiffness, na.rm = T),
                          ## myo_stiffness = mean(myo_stiffness, na.rm = T),
                          num_events = .N),
                        by = by]
  } else {

    all_measured_events[, displacement_nm := (displacement_bead_1_nm + displacement_bead_2_nm)/2 ]
    all_measured_events[, time_on_ms := (attachment_duration_bead_1_ms + attachment_duration_bead_2_ms)/2 ]
    all_measured_events[, step1 := (substep_1_bead_1_nm + substep_1_bead_2_nm)/2 ]
    all_measured_events[, step2 := (substep_2_bead_1_nm + substep_2_bead_2_nm)/2 ]

    all_measured_events[,
                        .(time_on_avg = mean(time_on_ms, na.rm = TRUE),
                        time_on_se = sd(time_on_ms, na.rm = TRUE)/sqrt(.N),
                        time_on_sd = sd(time_on_ms, na.rm = TRUE),
                        time_on_median = median(time_on_ms, na.rm = TRUE),
                        ## time_off_avg = mean(time_off_ms, na.rm = TRUE),
                        ## time_off_se = sd(time_off_ms, na.rm = TRUE)/sqrt(.N),
                        ## time_off_sd = sd(time_off_ms, na.rm = TRUE),
                        displacement_avg = mean(displacement_nm, na.rm = TRUE),
                        displacement_se = sd(displacement_nm, na.rm = TRUE)/sqrt(.N),
                        displacement_sd = sd(displacement_nm, na.rm = TRUE),

                        substep_1_avg = mean(step1, na.rm = TRUE),
                        substep_1_se = sd(step1, na.rm = TRUE)/sqrt(.N),
                        substep_1_sd = sd(step1, na.rm = TRUE),

                        substep_2_avg = mean(step2, na.rm = TRUE),
                        substep_2_se = sd(step2, na.rm = TRUE)/sqrt(.N),
                        substep_2_sd = sd(step2, na.rm = TRUE),

                        ## force_avg = mean(force, na.rm = TRUE),
                        ## force_se = sd(force, na.rm = TRUE)/sqrt(.N),
                        ## force_sd = sd(force, na.rm = TRUE),
                        ## trap_stiffness = mean(trap_stiffness, na.rm = T),
                        ## myo_stiffness = mean(myo_stiffness, na.rm = T),
                        num_events = .N),
    by = by]
  }
}


#' Calculate Total Time of Data Collection for each condition
#' @param project character. a lasertrapr project name
#' @param is_shiny logical
#' @import data.table
#' @export
total_trap_time <- function(project, is_shiny = FALSE){

  project_path <- file.path(path.expand("~"), "lasertrapr", project)

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

    get_time[[r]] <- data.table(conditions = filtered_options$conditions[[r]],
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




#' @import ggplot2 cowplot
#' @noRd
plot_ecdf <- function(measured_events, var, colorz, x_lab = "x_label", title,  basesize){

  gg <-
  ggplot(data = measured_events)+
    stat_ecdf(aes(base::get(var),
                  color = conditions),
              linewidth = 1,
              pad = FALSE,
              show.legend = FALSE)+
    scale_color_manual(values = colorz)+
    ylab("Cumulative Distribution")+
    xlab(x_lab)+
    ggtitle(title)+
    theme_cowplot(basesize)+
    theme(
      plot.title = element_text(hjust = 0.5, size = basesize)
    )
  return(gg)
}




#' Define a function to use mle to estimate detachment rate and boostrap CI
#' @import ggplot2 cowplot
#' @noRd
fit_attachment_durations <- function(data, colorz){

  fit_ton <- function(data){
                                        #from Schulte/Scott 2023 Biophysical Journal
                                        #get attachement times
    ton <- data$time_on_s
    fit_ton_pdf_mle <- function(ton, k){
                                        # pass the pars through to the negative log likelihood function
                                        # optim will optimize these
                                        # the variables ton will be inherited from parent function (no need to pass directly)
      nll_fun <- function(k){
                                        # PDF function from SPASM
        -sum(log(k*exp(-k*ton)))
      }

      fit <- optimize(nll_fun, lower = 0, upper = 100)

      return(fit)
    } #close fit_ton_pdf_mle

                                        # find k
    mod <- fit_ton_pdf_mle(ton = ton, k = 5)

                                        # extract fitted value from the model
    k <- mod$minimum[1]

                                        # function to generate fitted values to the exponential cumulative distribution
    fit_cdf <- function(x, k){ 1-exp(-k*x) }

                                        #calculate number of missing events
    n_missing <- fit_cdf(min(ton), k)*length(ton)

                                        #layout the range of x values for the real data bound by upper/lower bounds of data
    x_range <- seq(min(ton), max(ton), by = 1/1000)

                                        # generate the cdf values for the data
    cdf <- sapply(x_range, \(x) (sum(ton<=x)+n_missing) / (length(ton)+n_missing) )

    real_cdf <- data.frame(x = x_range,
                           y = cdf)



                                        # predict the fitted values from optimized points
    predict_x_range <- seq(0, max(ton), by = 1/1000)
    predict_y <- fit_cdf(k = k, x = predict_x_range)

    predict_df <- data.frame(x = predict_x_range,
                             y = predict_y)

#### BOOTSTRAP ####
    boostrap_ton_ci <- function(ton){

      boot_ton_fit <- function(ton){
        s <- sample(1:length(ton), replace = TRUE)
        new_ton <- ton[s]
        mod <- fit_ton_pdf_mle(ton = new_ton,
                               k = 5)
      } #close boot_ton_fit

      boot <- replicate(1000, boot_ton_fit(ton), simplify = FALSE)

      boot_df <- data.frame(k = sapply(boot, \(x) x$minimum[1]))

      ks <- sort(boot_df$k)

      k_lower <- ks[25]
      k_upper <- ks[975]

      return(list(boot_df = boot_df,
                  k_95 = c(k_lower, k_upper)))

    } #close bootstrap_ton_ci

    ci <- boostrap_ton_ci(ton = ton)

    k_low_diff <- round(mod$minimum[[1]] - ci$k_95[[1]], 2)
    k_high_diff <- round(ci$k_95[[2]] - mod$minimum[[1]], 2)

    html_label <- paste0(round(mod$minimum[1], 1),
                         " (-",
                         round(k_low_diff, 1),
                         "/+",
                         round(k_high_diff, 1),
                         ")")

    parse_label <- paste0(round(mod$minimum[1], 1),
                          "~(-",
                          round(k_low_diff, 1),
                          "/+",
                          round(k_high_diff, 1),
                          ")")

    list(
      data_df = real_cdf,
      mod = mod,
      rate = mod$minimum[1],
      predict_df = predict_df,
      boot_df = ci$boot_df,
      boot_ci = list(k1_low = k_low_diff,
                     k1_up = k_high_diff),
      html_label = html_label,
      parse_label = parse_label
    )
  }


                                        # fit the data
  ton_boot <-
    data |>
    dplyr::select(conditions, time_on_s)|>
    dplyr::group_by(conditions)|>
    tidyr::nest() |>
    dplyr::mutate(fit = purrr::map(data, fit_ton),
                  ton_rate = purrr::map(fit, `[[`, "rate"),
                  boot_ci = purrr::map(fit, `[[`, "boot_ci"),
                  predict = purrr::map(fit, `[[`, "predict_df"),
                  parse_label  = purrr::map_chr(fit, `[[`, "parse_label"),
                  html_label  = purrr::map_chr(fit, `[[`, "html_label"),
                  boot_df = purrr::map(fit, `[[`, "boot_df"),
                  cdf_data = purrr::map(fit, `[[`, "data_df"))


  ton_rate <-
    ton_boot |>
    dplyr::select(conditions, html_label)|>
    tidyr::unnest(cols = c( html_label))


  ## ton_ci <-
  ##   ton_boot |>
  ##   dplyr::select(conditions, boot_ci)|>
  ##   tidyr::unnest(cols = c(boot_ci))
                                        # unravel data from the nest back to long data for ggplot
                                        # gets the real emperical CDF
  ton_real_df <-
    ton_boot |>
    dplyr::select(conditions, cdf_data) |>
    tidyr::unnest(cols = c(cdf_data))

                                        # unravel data from the nest back to long data for ggplot
                                        # gets the fit prediction line
  ton_predict_df <-
    ton_boot |>
    dplyr::select(conditions, predict) |>
    tidyr::unnest(cols = c(predict))

                                        # make the plots
  ton_cdf <-
    ggplot()+
    geom_step(data = ton_real_df,
              aes(x = x,
                  y = y,
                  color = conditions),
              alpha = 0.6,
              linewidth= 1)+
    geom_line(data = ton_predict_df,
              aes(x = x,
                  y = y,
                  color = conditions),
              linetype = "dashed")+
    scale_color_manual(values = colorz)+
    ggtitle("Attachment Durations")+
    ylab("Cumulative Probability")+
    xlab("Time (s)")+
    theme_cowplot(16)+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none")

  return(list(
    gg = ton_cdf,
    rate = ton_rate,
    ton_real_df = ton_real_df,
    ton_predict_df = ton_predict_df,
    ton_boot = ton_boot
  )
  )
}
