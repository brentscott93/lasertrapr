# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "shinydashboard" )
## usethis::use_package("tidyverse", type = "depends")
usethis::use_package( "shinyWidgets" )
usethis::use_package( "purrr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "dplyr" )
usethis::use_package( "stringr" )
usethis::use_package( "dygraphs" )
usethis::use_package( "RcppRoll" )
usethis::use_package( "shinyjs" )
usethis::use_package( "pracma" )
usethis::use_package( "magrittr" )
usethis::use_package( "fresh" )
usethis::use_package( "changepoint" )
usethis::use_package( "gridExtra" )
usethis::use_package( "gt" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "lubridate" )
usethis::use_package( "hexbin" )
usethis::use_package( "depmixS4" )
usethis::use_package( "RcppRoll" )
usethis::use_package( "RColorBrewer" )
usethis::use_package( "rmarkdown" )
usethis::use_package( "data.table" )
usethis::use_package( "DT" )
usethis::use_package( "vroom" )
usethis::use_package( "glue" )
usethis::use_package( "survival" )
usethis::use_package( "survminer" )
usethis::use_package( "patchwork" )
usethis::use_package( "cowplot" )
usethis::use_package( "rstatix" )
usethis::use_package( "ggpubr" )
usethis::use_package( "Hmisc" )
usethis::use_package( "plotrix" )
usethis::use_package( "broom" )
usethis::use_package( "drc" )
usethis::use_package( "truncdist" )
usethis::use_package( "shinyFiles" )
usethis::use_package( "ggstatsplot" )
usethis::use_package( "colourpicker" )
usethis::use_package( "minpack.lm" )


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "mv" ) # Name of the module
golem::add_module( name = "split_obs" )
golem::add_module( name = "clean_data" )
golem::add_module( name = "folder_manager" )
golem::add_module( name = "hm_model" )
golem::add_module( name = "mini_ensemble" )
golem::add_module( name = "summarize" )
golem::add_module( name = "ensemble_average" )
golem::add_module( name = "figures" )
golem::add_module( name = "isometric_force_clamp" )
golem::add_module( name = "covariance" )

#golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )
golem::add_fct( "split_obs" ) 
golem::add_utils( "clean_data" )
golem::add_fct( "clean" ) 
golem::add_utils( "theme" )
golem::add_fct( "calibrations" ) 
golem::add_fct( "mini_ensemble_analyzer" ) 
golem::add_fct( "ensemble_average" ) 
golem::add_utils( "ensemble_average" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("lasertrapr")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

