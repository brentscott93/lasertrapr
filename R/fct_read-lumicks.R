#' @noRd
read_lumicks <- function(input_data,
                         project,
                         conditions,
                         date,
                         downsample_by=5){


    withProgress(message = 'Initializing Data', value = 0, {
       browser()
        for(r in seq_along(input_data$datapath)){

            shiny::incProgress(1/nrow(input_data))


          lumicks <- rhdf5::h5read(input_data$datapath[[r]], name = "Force HF")

          l_dt <- data.table(t1 = lumicks$`Force 1x`,
                             t2 = lumicks$`Force 2x`)

          ## cal <- rhdf5::h5read(input_data$datapath[[r]], name = "Calibration/1")

          ## cal1 <- cal$`Force 1x`
          ## cal2 <- cal$`Force 2x`

          orig_hz <- 78125
          new_hz <- round(orig_hz/downsample_by)
          downsample_points <- seq(1, nrow(l_dt), by = downsample_by)
          l_dt <- l_dt[downsample_points]

          t <- data.table(project = project$name,
                          conditions = conditions$name,
                          date = date$name,
                          obs = NA,
                          raw_bead_1 = l_dt$t1,
                          raw_bead_2 = l_dt$t2)


             o <- data.frame(project = project$name,
                             conditions = conditions$name,
                             date = date$name,
                             obs = NA,
                             hz = new_hz,
                             processor = NA,
                             include = NA,
                             mv2nm = NA,
                             nm2pn = NA,
                             mv2nm2 = NA,
                             nm2pn2 = NA,
                             analyzer = NA,
                             report = 'not run',
                             review = NA,
                             channels = 2,
                             lab = "lumicks",
                             original_filename = input_data$datapath[[r]])


          if(r < 10){
            obs <- paste0("obs-0", r)
          } else {
            obs <- paste0("obs-", r)
          }

            dir.create(file.path(date$path, obs))

            t$obs <- obs
            data.table::fwrite(t, file = file.path(date$path, obs, "trap-data.csv"), sep = ",")

            o$obs <- obs
            data.table::fwrite(o, file = file.path(date$path, obs, "options.csv"), sep = ",")


              }
    }
    )

}
