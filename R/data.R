#' Simulated laser trap data from the app
#'
#'
#' @format ## `simulated_trap_data`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{data}{bead position in data points}
#'   \item{key, key_value, state}{baseline or event state}
#'   \item{time}{seconds}
#'   \item{project, conditions, date, obs}{metadata}
#'   \item{raw_bead, processed_bead}{converted bead position from simulator to work with app format}
#'   ...
#' }
#' @source Generated from within the app
"simulated_trap_data"
