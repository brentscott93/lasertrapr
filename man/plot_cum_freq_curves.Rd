% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_summarize.R
\name{plot_cum_freq_curves}
\alias{plot_cum_freq_curves}
\title{Contruct Cumulative Frequency Curves}
\usage{
plot_cum_freq_curves(data, type)
}
\arguments{
\item{data}{df}

\item{type}{time_on_ms or time_off_ms}
}
\value{
a list with a df and plot
}
\description{
Contruct Cumulative Frequency Curves
}
\examples{
plot_cum_freq_curves(event_files_filtered, time_on_ms)
}
