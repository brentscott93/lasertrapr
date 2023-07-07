## library(data.table)
## library(dygraphs)


## datt <- fread("/run/media/brent/50FABD0BFABCEE7A/spudich-harmonic-force/HFS_v3_public/hfs-dummy1.txt")
## datt1 <- fread("/run/media/brent/50FABD0BFABCEE7A/spudich-harmonic-force/HFS_v3_public/hfs-dummy2.txt")
## read_greenberg <- function(file){

##   raw_data <- fread(file, skip = 68)
##   header_data <- fread(file, nrows = 67, header = FALSE)

##   header_line_numbers <- c(15, 18, 20, 22, 24)
##   header_data <- header_data[header_line_numbers,]
##   options <- as.list(as.numeric(header_data$V2))
##   names(options) <- c("hz", "pn_nm1", "pn_nm2", "nm_v1", "nm_v2")

##   list(data = raw_data,
##        options = as.data.frame(options))

## }


##   user_amp_thresh <- 60
##   user_phase_thresh <- 1


## dat <- read_greenberg("~/Downloads/221027_f2_pca10_harmonic-force 29.txt")
## dat$data <- dat$data[44000:102000]

## dygraph(data.frame(x = 1:nrow(dat$data),
##                    y = dat$data$Trap1X*dat$options$nm_v1,
##                    stage = (dat$data$StagePos-mean(dat$data$StagePos))*1000))|>
##   dyRangeSelector()


## ## convert bead position to nm and center baseline around 0
## ## centering should be done in clean and process...
## ## but this will get close for now
## bead <- datt$V3*(114/80)
## stage <- datt$V6*-1
## plot(bead[1:10000], type = "l")
## ## stage <- (dat$data$StagePos-mean(dat$data$StagePos))*1000
## ## bead <- dat$data$Trap1X*dat$options$nm_v1
## ## bead <- bead-mean(bead)

## ## spectrum(bead)
## ## stage_spectrum <- spectrum(stage_win, plot = FALSE)
## ## stage_hz <- round(1/ssp$freq[ssp$spec==max(ssp$spec)], 0)
## ## baed rolling average over a period
## ## bead_roll <- na.omit(RcppRoll::roll_meanl(bead, n = 100, by = 1))

## ## dygraph(data.table(1:length(bead_roll),
## ##                    bead_roll))

## # move this later into function par
## hz <- 40000
## hz_4 <- hz/4

## thresh_win <- vector("list")
## amp_win <- vector("list")
## phase_win <- vector("list")
## bead_roll_win <- vector("list")
## win <- seq(1, length(bead), by = hz_4)
## for(dow in seq_along(win)){
##   bead_win <- na.omit(bead[win[dow]:(win[dow]+hz_4-1)])
##   bead_win <- bead_win-mean(bead_win)
##   stage_win <- na.omit(stage[win[dow]:(win[dow]+hz_4-1)])
##   bead_roller <- na.omit(RcppRoll::roll_meanl(bead_win, n = per, by = 1))

##   plot(bead_win, type = "l")
##   plot(stage_win, type = "l")

##   ## fit the stage with a sine function
##   ## thanks to Glen_b in 2013 from https://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
##   t <- 1:length(stage_win)
##   ## get the frequency/period info of the stage oscillation
##   ssp <- spectrum(stage_win, plot = TRUE)
##   ## per <- stage_hz
##   per <- round(1/ssp$freq[ssp$spec==max(ssp$spec)], 0)
##   ## per <- 100
##                                         ## model the stage position using linear regression
##   reslm <- lm(stage_win ~ sin(2*pi/per*t)+cos(2*pi/per*t))
##   summary(reslm)
##   rg <- diff(range(stage_win))
##   plot(stage_win~t,ylim=c(min(stage)-0.1*rg,max(stage)+0.1*rg), type = "l")
##     lines(fitted(reslm), col = 4, type = "l")

##   fit_stage <- fitted(reslm)-mean(fitted(reslm))
##   plot(fit_stage, type = "l")


##   stage_sin <- fitted(reslm)-mean(stage_sin)


##   ## calculate a cosine wave by phase shifting sine fit
##   ## phase shift is by pi/2 which is period/2
##   stage_cos <- c(stage_sin[-c(1:(per/2))], stage_sin[1:(per/2)])

##   bead_sin <- stage_sin*bead_win
##   bead_cos <- stage_cos*bead_win

##   ## Sung & Spudich call this s(t)
##   bead_sin_roll <- na.omit(RcppRoll::roll_meanl(bead_sin, n = per, by = 1))
##   ## c(t)
##   bead_cos_roll <- na.omit(RcppRoll::roll_meanl(bead_cos, n = per, by = 1))
##   ## amplitude = sqrt ( s(t)^2 + c(t)^2 )
##   amp <- sqrt((bead_sin_roll^2) + (bead_cos_roll^2))
##   ## amp2 <- amp %% 2*pi

##   ## plot(stage_sin %% 2*pi, type = "l")
##   ## lines(stage_sin %% 2*pi, type = "l", col=4)
##   ## plot(amp2, typ = "l")
##   ## plot(bead_win)
##   ## phase = arctan(c(t) / s(t) )
##   ## arctan is inverse tangent
##   phase2 <- atan(bead_cos_roll / bead_sin_roll)
##   ## from Kim Mortensens HFC analyzer script
##   ## i think this normalizes the actan to give directionality
##   phase <- vector()
##   for(p in seq_along(bead_sin_roll)){
##     if(bead_sin_roll[p] > 0 && bead_cos_roll[p] > 0){
##       phase[p] <- atan(bead_cos_roll[p]/bead_sin_roll[p])
##     } else if(bead_sin_roll[p] < 0 && bead_cos_roll[p] > 0){
##       phase[p] <- atan(bead_cos_roll[p] / bead_sin_roll[p]) + pi
##     } else if(bead_sin_roll[p] > 0 && bead_cos_roll[p] < 0){
##       phase[p] <- atan(bead_cos_roll[p] / bead_sin_roll[p])
##     } else if (bead_sin_roll[p] < 0 && bead_cos_roll[p] < 0){
##       phase[p] <- atan(bead_cos_roll[p] / bead_sin_roll[p]) - pi
##     }
##   }


##   amp_thresh <- ifelse(amp >= user_amp_thresh, 1, 0)
##   phase_thresh <- ifelse(abs(phase) >= user_phase_thresh, 1, 0)

##   sum_thresh <- amp_thresh+phase_thresh
##   sum_thresh <- ifelse(sum_thresh == 2, 1, 0)
##   thresh_win[[dow]] <- sum_thresh
##   amp_win[[dow]] <- amp
##   phase_win[[dow]] <- phase
##   bead_roll_win[[dow]] <- bead_roller

## }

## thresh <- unlist(thresh_win)
## amp <- unlist(amp_win)
## amp_dt <- data.table(amp = amp)
## amp_dt[, index := .I]
## phase <- unlist(phase_win)
## bead_roll <- unlist(bead_roll_win)

## thresh_rle <- as.data.table(unclass(rle(thresh)))
## thresh_rle[, end_event := cumsum(lengths)]
## thresh_rle[, time_on_dp := end_event - c(0, head(end_event, -1))]
## thresh_rle[, begin_event := (end_event-time_on_dp)+1]
## thresh_rle[, `:=`(is_event = time_on_dp >= per & values == 1,
##                   index = .I)]

## thresh_events <- thresh_rle[is_event == TRUE]



## absolute_displacements <- vector()
## relative_displacements <- vector()
## average_baseline_before <- vector()
## average_baseline_after <- vector()
## ## attachment_durations_dp <- vector()
## event_amplitudes <- vector()
## for(event in 1:nrow(thresh_events)){

##   event_start <- thresh_events$begin_event[[event]]+(per/2)
##   event_stop <- thresh_events$end_event[[event]]-(per/2)

##   if(thresh_events$index[[event]] == 1){

##     absolute_displacements[[event]] <- NA
##     relative_displacements[[event]] <- NA
##     average_baseline_before[[event]] <- NA
##     average_baseline_after[[event]] <- NA
##     ## attachment_durations_dp[[event]] <- NA
##     event_amplitudes[[event]] <- NA

##     next
##   }
##   before_baseline_row <- thresh_events$index[[event]] - 1
##   before_baseline_dt <- thresh_rle[before_baseline_row]
##   before_baseline_start <- before_baseline_dt$begin_event[[1]] + (per/2)
##   before_baseline_stop <- before_baseline_dt$end_event[[1]] - (per/2)

##   if(thresh_events$index[[event]] == nrow(thresh_rle)){

##     absolute_displacements[[event]] <- NA
##     relative_displacements[[event]] <- NA
##     average_baseline_before[[event]] <- NA
##     average_baseline_after[[event]] <- NA
##     ## attachment_durations_dp[[event]] <- NA
##     event_amplitudes[[event]] <- NA

##     next
##   }
##   after_baseline_row <- thresh_events$index[[event]] + 1
##   after_baseline_dt <- thresh_rle[after_baseline_row]
##   after_baseline_start <- after_baseline_dt$begin_event[[1]] + (per/2)
##   after_baseline_stop <- after_baseline_dt$end_event[[1]] - (per/2)

##   ## calcualts bead position
##   before_mean_baseline <- mean(bead_roll[before_baseline_start:before_baseline_stop])
##   after_mean_baseline <- mean(bead_roll[after_baseline_start:after_baseline_stop])

##   absolute_bead_position <- mean(bead_roll[event_start:event_stop])
##   relative_bead_position <- absolute_bead_position - before_mean_baseline


##   #calculate amp
##   before_amp_baseline <- mean(amp[before_baseline_start:before_baseline_stop])
##   after_amp_baseline <- mean(amp[after_baseline_start:after_baseline_stop])

##   absolute_mean_amp <- mean(amp[event_start:event_stop])
##   relative_mean_amp <- absolute_mean_amp - before_amp_baseline


##   absolute_displacements[[event]] <- absolute_bead_position
##   relative_displacements[[event]] <- relative_bead_position
##   average_baseline_before[[event]] <- before_mean_baseline
##   average_baseline_after[[event]] <- after_mean_baseline
##   ## attachment_durations_dp[[event]] <- thresh_events$lengths[[event]]


##   ## delta_f <-  relative_mean_amp * dat$options$pn_nm1
##   ## all_delta_f[[event]] <- delta_f
##   event_amplitudes[[event]] <- mean(bead[event_start:event_stop])

##   ## determine time on
##   ## peak_amp_half <- max(amp[event_start:event_stop])/2
##   ## start_time_on_look <- thresh_events$begin_event[[event]]-(2*per)
##   ## stop_time_on_look <- thresh_events$begin_event[[event]]+(2*per)

##   ## amp_roll_subset <- amp_dt[start_time_on_look:stop_time_on_look]


##   ## t1 <- amp_roll_subset[which(amp_roll_subset$amp >= peak_amp_half)]$index[[1]]



##   ## start_time_on_look2 <- thresh_events$end_event[[event]]-(2*per)
##   ## stop_time_on_look2 <- thresh_events$end_event[[event]]+(2*per)

##   ## amp_roll_subset2 <- amp_dt[start_time_on_look2:stop_time_on_look2]
##   ## t2 <- amp_roll_subset2[which(amp_roll_subset2$amp <= peak_amp_half)]$index[[1]]

##   ## before_amp_avg <- mean(amp[before_baseline_start:before_baseline_stop])
##   ## after_amp_avg <- mean(amp[after_baseline_start:after_baseline_stop])
##   ## baseline_amp_avg <- mean(c(before_amp_avg, after_amp_avg))


## }

## absolute_force <- absolute_displacements * dat$options$pn_nm1
## relative_force <- relative_displacements * dat$options$pn_nm1

## final_dt <- thresh_events[, .(begin_event_dp = begin_event,
##                               end_event_dp = end_event,
##                               absolute_displacements_nm = absolute_displacements,
##                               relative_displacements_nm = relative_displacements,
##                               absolute_force_pn = absolute_force,
##                               relative_force_pn = relative_force,
##                               average_baseline_bead_before = average_baseline_before,
##                               average_baseline_bead_after = average_baseline_after,
##                               average_amplitude_delta_f = event_amplitudes)]

## roll_bead <- data.table(x = 1:length(bead_roll), bead_roll = bead_roll )

## diff_len <- length(bead_roll)-length(amp)

## hfs_roll <- data.table(
##                        ## x= 1:length(bead),
##                        ## bead = bead
##                        ## seconds = 1:length(bead_roll)/hz,
##                        ## bead_roll = bead_roll
##                          t = 1:length(amp),
##                        amp_roll = amp
##                        ## phase_roll = phase
##                        )

## add_shades <- function(x, periods, ...){
##   for(p in 1:nrow(periods)){
##     x <- dygraphs::dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], color = periods$color[[p]], ...)
##   }
##   x
## }

## periods <- data.table(start = final_dt$begin_event_dp, stop = final_dt$end_event_dp, color = "red")

## dygraph(hfs_roll) |>
##   dyRangeSelector() #|>
##   add_shades(periods)


## dygraph(roll_bead) |>
##   dyRangeSelector()


## ggplot()+
##   geom_point(aes(phase, amp))
