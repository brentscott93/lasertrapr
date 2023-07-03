library(data.table)
library(dygraphs)

read_greenberg <- function(file){

  raw_data <- fread(file, skip = 68)
  header_data <- fread(file, nrows = 67, header = FALSE)

  header_line_numbers <- c(15, 18, 20, 22, 24)
  header_data <- header_data[header_line_numbers,]
  options <- as.list(as.numeric(header_data$V2))
  names(options) <- c("hz", "pn_nm1", "pn_nm2", "nm_v1", "nm_v2")

  list(data = raw_data,
       options = as.data.frame(options))

}


dat <- read_greenberg("~/Downloads/221027_f2_pca10_harmonic-force 29.txt")
dat$data <- dat$data[44000:102000]

dygraph(data.frame(x = 1:nrow(dat$data),
                   y = dat$data$Trap1X*dat$options$nm_v1,
                   stage = (dat$data$StagePos-mean(dat$data$StagePos))*1000))|>
  dyRangeSelector()


## convert bead position to nm and center baseline around 0
## centering should be done in clean and process...
## but this will get close for now
stage <- (dat$data$StagePos-mean(dat$data$StagePos))*1000
bead <- dat$data$Trap1X*dat$options$nm_v1
bead <- bead-mean(bead)

# move this later into function par
hz <- 20000
hz_4 <- hz/4

bead <- bead
bead <- bead-mean(bead)
stage <- stage

plot(bead, type = "l")
plot(stage, type = "l")

## fit the stage with a sine function
## thanks to Glen_b in 2013 from https://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
t <- 1:length(stage)
# get the frequency/period info of the stage oscillation
ssp <- spectrum(stage)
per <- round(1/ssp$freq[ssp$spec==max(ssp$spec)], 0)
# model the stage position using linear regression
reslm <- lm(stage ~ sin(2*pi/per*t)+cos(2*pi/per*t))
## summary(reslm)
## rg <- diff(range(y))
## plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
stage_sin <- predict(reslm)

# calculate a cosine wave by phase shifting sine fit
# phase shift is by pi/2 which is period/2
stage_cos <- c(stage_sin[-c(1:(per/2))], stage_sin[1:(per/2)])

bead_sin <- stage_sin*bead
bead_cos <- stage_cos*bead

## plot(stage_sin, type = "l")
## lines(stage_cos, type = "l", col=4)


## Sung & Spudich call this s(t)
bead_sin_roll <- na.omit(RcppRoll::roll_meanl(bead_sin, n = per, by = 1))
## c(t)
bead_cos_roll <- na.omit(RcppRoll::roll_meanl(bead_cos, n = per, by = 1))



## amplitude = sqrt ( s(t)^2 + c(t)^2 )
amp <- sqrt((bead_sin_roll^2) + (bead_cos_roll^2))
## phase = arctan(c(t) / s(t) )
## arctan is inverse tangent
phase <- atan(bead_cos_roll / bead_sin_roll)

amp_diff <- head(amp, -1) - tail(amp, -1)

par(mfrow = c(4, 1))
plot(bead, type = "l")
plot(stage, type = "l")
plot(amp, type = "l")
plot(phase, type = "l")

## plot(amp_diff, type = "l")


dygraph(data.frame(x = 1:nrow(dat$data),
                   y = dat$data$Trap1X*dat$options$nm_v1,
                   stage = (dat$data$StagePos-mean(dat$data$StagePos))*1000),
        group = "group")

## dygraph(data.frame(x = 1:(length(bead_sin_roll)-1),
##                    amp = diff(amp)),
##         group = "group")


## dygraph(data.frame(x = 1:length(bead_sin_roll)/200,
##                    amp = amp),
##         group = "group")

## dygraph(data.frame(x = 1:length(bead_sin_roll),
##                    phase = phase),
##         group = "group")|>
##   dyRangeSelector()

## wave_dt <- data.table(amp = amp,
##                       phase = phase)

user_amp_thresh <- 50
user_phase_thresh <- 0.3

amp_thresh <- ifelse(amp >= user_amp_thresh, 1, 0)
phase_thresh <- ifelse(abs(phase) >= user_phase_thresh, 1, 0)

sum_thresh <- amp_thresh+phase_thresh
sum_thresh <- ifelse(sum_thresh == 2, 1, 0)

thresh_rle <- as.data.table(unclass(rle(sum_thresh)))
thresh_rle[, cs_length := cumsum(lengths)]
thresh_rle[, rel_change := cs_length - c(0, head(cs_length, -1))]
thresh_rle[, begin_event := (cs_length-rel_change)+1]
thresh_rle[, `:=`(is_event = rel_change >= per & values == 1,
                  index = .I)]

thresh_events <- thresh_rle[is_event == TRUE]

absolute_displacements <- vector()
relative_displacements <- vector()
average_baseline <- vector()
attachment_durations_dp <- vector()
event_amplitudes <- vector()
all_delta_f <- vector()
for(event in 1:nrow(thresh_events)){

  event_start <- thresh_events$begin_event[[event]]+(per/2)
  event_stop <- thresh_events$cs_length[[event]]-(per/2)

  before_baseline_row <- thresh_events$index[[event]] - 1
  before_baseline_dt <- thresh_rle[baseline_row]
  before_baseline_start <- baseline_dt$begin_event[[1]] + (per/2)
  before_baseline_stop <- baseline_dt$cs_length[[1]] - (per/2)

  after_baseline_row <- thresh_events$index[[event]] + 1
  after_baseline_dt <- thresh_rle[baseline_row]
  after_baseline_start <- baseline_dt$begin_event[[1]] + (per/2)
  after_baseline_stop <- baseline_dt$cs_length[[1]] - (per/2)

  mean_baseline <- mean(bead[baseline_start:baseline_stop])
  absolute_mean_amp <- mean(bead[event_start:event_stop])
  relative_mean_amp <- absolute_mean_amp - mean_baseline

  absolute_displacements[[event]] <- absolute_mean_amp
  relative_displacements[[event]] <- relative_mean_amp
  average_baseline[[event]] <- mean_baseline
  ## attachment_durations_dp[[event]] <- thresh_events$lengths[[event]]


  delta_f <-  relative_mean_amp * dat$options$pn_nm1
  all_delta_f[[event]] <- delta_f

  ## determine time on
  peak <- max(amp[event_start:event_stop])

}

par(mfrow = c(4, 1))
plot(bead, type = "l")
points(x = thresh_events$begin_event, y = rep(-50, nrow(thresh_events)), col=5)
points(x = thresh_events$cs_length, y = rep(-50, nrow(thresh_events)), col=2)
plot(stage, type = "l")
plot(amp, type = "l")
plot(phase, type = "l")



wave_dt[, index := .I]

lines((t[1:500]/per-(pi/2))*per/20000, stage_sin[1:500], col=6)
lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit

dygraph(data.frame(t = t,
                   y = y,
                   f = fitted(reslm)))

plot(y[seq(1, length(y), by = 5)])


spec <- spec.ar(y, plot = FALSE, n.freq = 1000)
f <- spec$freq[which.max(spec$spec)]
fm <- nls(y ~ cbind(C = 1, A = cos(omega*t + phi)),
          start = list(omega =  2*pi*f, phi = 0), algorithm = "plinear")

fm
## Nonlinear regression model
##   model: y ~ cbind(C = 1, A = cos(omega * t + phi))
##    data: parent.frame()
##   omega     phi  .lin.C  .lin.A
##  0.2636  0.3819 10.1501  2.5838
##  residual sum-of-squares: 22.99
##
## Number of iterations to convergence: 5
## Achieved convergence tolerance: 2.346e-06

plot(y ~ t)
lines(fitted(fm) ~ t)


library(ggplot2)


ggplot()+
  geom_point(aes(amp, phase))
