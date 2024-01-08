                                        # Set options here
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
## trap_data_files <- list.files("~/lasertrapr/project_lasertrapr-paper_mini-ensemble/25_ug-ml",
##                               pattern = "trap-data.csv",
##                               full.names = TRUE,
##                               recursive = TRUE)



## options_files <- list.files("~/lasertrapr/project_lasertrapr-paper_mini-ensemble/25_ug-ml",
##                               pattern = "options.csv",
##                               full.names = TRUE,
##                               recursive = TRUE)


## file_list <- c(trap_data_files, options_files)

## for(i in seq_along(file_list)){
##   print(i)
##   fname <- file_list[[i]]
##   f <- data.table::fread(fname)
##   ## f[,
##   ##   ## project = "project_lasertrapr-paper_mini-ensemble",
##   ##   ## conditions = "25_ug-ml",
##   ##   date  := "2022-02-22"
##   ## ]
##   f$date <- "2022-02-22"
##   data.table::fwrite(f, fname)
## }
