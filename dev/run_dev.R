# Set options here
options(golem.app.prod = FALSE,
        browser = "firefox") # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# tell shiny to log all reactivity
reactlog::reactlog_enable()
# Run the application
run_app()

shiny::reactlogShow()
