
options(golem.app.prod = FALSE,
        browser = "firefox") # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

                                        # Document and reload your package
golem::document_and_reload()

# tell shiny to log all reactivity
## reactlog::reactlog_enable()
# Run the application
run_app()

## shiny::reactlogShow()

#rename project and conditions
## trap_data_files <- list.files("~/lasertrapr/project_dani-OM-only-copied",
##                               pattern = "trap-data.csv",
##                               full.names = TRUE,
##                               recursive = TRUE)



## options_files <- list.files("~/lasertrapr/project_dani-OM-only-copied",
##                               pattern = "options.csv",
##                               full.names = TRUE,
##                               recursive = TRUE)


## me_files <- list.files("~/lasertrapr/project_dani-OM-only-copied",
##                               pattern = "measured-events.csv",
##                               full.names = TRUE,
##                               recursive = TRUE)
## name
## file_list <- c(trap_data_files, options_files, me_files)

## for(i in seq_along(file_list)){
##   print(i)
##   fname <- file_list[[i]]
##   f <- data.table::fread(fname)
##   ## f[,
##   ##   project = "project_dani-single-molecule_1uM-atp",
##   ##   ## conditions = "25_ug-ml",
##   ##   ## date  := "2022-02-22"
##   ## ]
##   ## f$date <- "2022-02-22"
##   f$project <- "project_dani-OM-only-copied"
##   ## f$conditions <- "10uM-dani_1mM-atp"
##   data.table::fwrite(f, fname)
## }




## measured_events <- data.table::fread("~/lasertrapr/project_lasertrapr-paper-fig2-simulations/summary/2025-12-22_project_lasertrapr-paper-fig2-simulations_all-measured-events.csv")

## fit_normal_variable(measured_events)
##
## cname

## gen_rnorm(measured_events, cname)
