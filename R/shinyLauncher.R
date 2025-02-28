shinyLauncher_R <- '
# shinyLauncher.R
# Provides a function to run the Shiny App located in inst/shiny-app
# Launch the SWDSFODR Shiny Application
#
# @description
#   Opens a Shiny GUI where the user can input data for the FOD model, 
#   select approach (annual / monthly / simplified), and see interactive charts/tables.
#
# @export
runSWDSFODRApp <- function() {
# Create directories first
dir.create("SWDSFODR", showWarnings = FALSE)
dir.create("SWDSFODR/R", showWarnings = FALSE)
  appDir <- system.file("shiny-app", package="SWDSFODR")
  if (appDir == "") {
    stop("Cannot find Shiny app. Try re-installing `SWDSFODR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
'
writeLines(shinyLauncher_R, "SWDSFODR/R/shinyLauncher.R")
