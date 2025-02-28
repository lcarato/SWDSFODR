shinyLauncher_R <- '
# shinyLauncher.R
# Provides a function to run the Shiny App located in inst/shiny-app

# Launch the SWDSFOD Shiny Application
#
# @description
#   Opens a Shiny GUI where the user can input data for the FOD model, 
#   select approach (annual / monthly / simplified), and see interactive charts/tables.
#
# @export
runSWDSFODApp <- function() {
  appDir <- system.file("shiny-app", package="SWDSFOD")
  if (appDir == "") {
    stop("Cannot find Shiny app. Try re-installing `SWDSFOD`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
'
writeLines(shinyLauncher_R, "SWDSFODR/R/shinyLauncher.R")
