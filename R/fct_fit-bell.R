##' Fit bell equation with mle and bootstrap CI
##' @param force numeric vector containing the x-axis force measurements. Must be same length as time and id.
##' @param time numeric vector containing the y-axis time measurements. Must be same lenfth as force and id.
##' @param id NULL (default), or a character vector denoting which group the data is in. Must be same length as force and time.
##' @param tmin numeric specifying minimum attachment time (dead time)
##' @param plot_colors a character vector specyfing plot colors
##' @param basesize numeric specifying base size for plot
##' @param textsize numeric specifying text size for plot
##' @return a list containing "plot" and "boot_data". "plot" - is a ggplot2 object. "boot_data" - is a nested data frame containing information about the fits and bootstrapping.
##' @import ggplot2 ggtext cowplot
##' @export
fit_force_clamp <- function(measured_events, tmin = NULL, plot_colors = NULL, basesize = 12, textsize = 10, is_shiny = FALSE){

  ## if(!length(time)==length(force)){
  ##   stop("The vectors passed to force and time must be equal in length")
  ## }

  ## if(!is.null(id)){
  ##   if(!length(time)==length(id)){
  ##     stop("The vectors passed to ID must be must be equal in length to force and time.")
  ##   }
  ## }

  data <-  measured_events[, .(conditions = conditions,
                               force_avg = force_pn,
                               time_on_s = time_on_s) ]

  if(tmin == 0) tmin <- NULL
    if(!is.null(tmin)){
      data <- data[which(data$time_on_s >= tmin),]
    }
  ## if(!is.null(id)){
  ##   data$conditions <- id
  ## }

  if(is.null(plot_colors)){
    plot_colors <- unname(palette.colors())
  }
  ## print(paste("tmin-in-fun="), tmin)
  fit_bell <- function(data, tmin){


    fit_bell_pdf_mle <- function(F, t, pars, tmin){
      ## pass the pars through to the negative log likelihood function
      ## optim will optimize these
      ## the variables F, t will be inherited from parent function (no need to pass directly)
      nll_fun <- function(pars, tmin){
        k0 <- pars[1]
        d <- pars[2]
        if(is.null(tmin)){
          ## PDF function from MEMLET https://github.com/michaelswoody/MEMLET/blob/master/Matlab%20Code/MEMLET/PDFs/bellsEqn.m
          -sum(log(((k0*exp(-(F*d)/4.1))*exp(-(k0*exp(-(F*d)/4.1))*t))))
        } else {
           -sum(
             log(
             ((k0*exp(-(F*d)/4.1))*exp(-(k0*exp(-(F*d)/4.1))*t)) / (exp(-(k0*exp(-(F*d)/4.1))*tmin))
             )
           )
        }
      }

      fit <- optim(pars, nll_fun, tmin = tmin)
      return(fit)
    } #close fit_bell_pdf_mle

    mod <- fit_bell_pdf_mle(F = data$force_avg, t = data$time_on_s, pars = c(50, 1), tmin = tmin)

    predict_bell <- function(mod, F){
      k0 <- 1/mod$par[1]
      d <- mod$par[2]
      dummy_F <- seq(min(F), max(F), by = 1/100)
      t <- k0*exp((dummy_F*d)/4.1)
      predict_df <- data.frame(F = dummy_F,
                               t = t)
    } #close predict_bell

    predict_df <- predict_bell(mod, data$force_avg)

    ## bootstrap
    boostrap_bell_ci <- function(F, t, tmin){

      data <- data.frame(F, t)

      boot_bell_fit <- function(data, tmin){
         if(is_shiny) incProgress(1/5000, detail = "Bootstrapping...")
        s <- sample(1:nrow(data), replace = TRUE)
        df <- data[s,]
        mod <- fit_bell_pdf_mle(F = df$F,
                                t = df$t,
                                pars = c(50, 1),
                                tmin = tmin)
      } #close boot_bell_fit

      boot <- replicate(1000, boot_bell_fit(data, tmin), simplify = FALSE)

      boot_df <- data.frame(k0 = sapply(boot, \(x) x$par[1]),
                            d = sapply(boot, \(x) x$par[2]))

      k0s <- sort(boot_df$k0)
      ds <- sort(boot_df$d)

      k0_lower <- k0s[25]
      k0_upper <- k0s[975]

      d_lower <- ds[25]
      d_upper <- ds[975]

      return(list(boot_df = boot_df,
                  k0_95 = c(k0_lower, k0_upper),
                  d_95 = c(d_lower, d_upper)))

    } #close bootstrap_bell_ci

    ci <- boostrap_bell_ci(data$force_avg, data$time_on_s, tmin)

    ci_low <- list(par = c(ci$k0_95[2], ci$d_95[1]))
    ci_high <- list(par = c(ci$k0_95[1], ci$d_95[2]))

    k0_low_diff <- round(mod$par[[1]] - ci$k0_95[[1]], 0)
    k0_high_diff <- round(ci$k0_95[[2]] - mod$par[[1]], 0)
    d_low_diff <- format(round(mod$par[[2]] - ci$d_95[[1]], 2), nsmall = 2)
    d_high_diff <- format(round(ci$d_95[[2]] - mod$par[[2]], 2), nsmall = 2)

    label <- paste0("k<sub>0</sub> = ",
                    round(mod$par[1], 0),
                    " (+",
                    k0_high_diff,
                    "/-",
                    k0_low_diff,
                    ") s<sup>-1</sup>",
                    "<br>",
                    "d = ",
                    round(mod$par[2], 2),
                    " (+",
                    d_high_diff,
                    "/-",
                    d_low_diff,
                    ") nm")

    list(
      mod = mod,
      predict_df = predict_df,
      boot_df = ci$boot_df,
      boot_ci = list(k1_low = k0_low_diff,
                     k1_up = k0_high_diff,
                     d_low = d_low_diff,
                     d_up = d_high_diff),
      label = label
    )
  }


  bell_df <-
    data |>
    dplyr::group_by(conditions) |>
    tidyr::nest() |>
    dplyr::mutate(fit = purrr::map(data, ~fit_bell(.x, tmin = tmin)),
                  predict = purrr::map(fit, `[[`, "predict_df"),
                  label  = purrr::map_chr(fit, `[[`, "label"),
                  n = purrr::map_dbl(data, nrow)
                  )
#for exporting boostrap values
  boot_bell_df <-
    bell_df |>
    dplyr::mutate(boot = purrr::map(fit, `[[`, "boot_df"),
                  label  = purrr::map_chr(fit, `[[`, "label")
                  ) |>
    dplyr::select(conditions, boot)|>
    tidyr::unnest(cols = boot)

  bell_df <- dplyr::mutate(bell_df, label = paste0("<b>",
                                                   conditions,
                                                   " (n = ", n, ")</b><br>",
                                                   label))

  ## unravel prediction line from the nest
  predict_df <-
    bell_df |>
    dplyr::select(conditions, predict) |>
    tidyr::unnest(cols = c(predict))


  gg_bell <-
    ggplot()+
    geom_point(data = data,
               aes(x = force_avg,
                   y = time_on_s,
                   color = conditions),
               alpha = 0.5,
               shape = 16,
               size = 2)+
    geom_richtext(data = bell_df,
                  aes(x = -Inf,
                      y = Inf,
                      label = label,
                      color = conditions),
                  label.color = "white",
                  fill = NA,
                  hjust = 0,
                  vjust = 1,
                  size = textsize/.pt)+
    facet_wrap(~conditions)+
    scale_color_manual(values = plot_colors)+
    ylab("Time (s)")+
    xlab("Force (pN)")+
    scale_y_log10()+
    theme_cowplot(basesize)+
    theme(
      legend.position = "none",
      strip.background = element_rect("white"),
      strip.text = element_blank()
    )

  return(
    list(
      data = data,
      boot_data = bell_df,
      boot_params = boot_bell_df,
      plot = gg_bell)
  )
}
