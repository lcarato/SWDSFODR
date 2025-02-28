# Create directories first
dir.create("SWDSFODR", showWarnings = FALSE)
dir.create("SWDSFODR/R", showWarnings = FALSE)

# shinyLauncher.R
shiny_launcher_R <- "#' Launch the SWDSFODR Shiny Application
#'
#' @description
#'   Opens a Shiny GUI where the user can input data for the FOD model, 
#'   select approach (annual / monthly / simplified), and see interactive charts/tables.
#'
#' @export
runSWDSFODRApp <- function() {
  appDir <- system.file(\"shiny-app\", package=\"SWDSFODR\")
  if (appDir == \"\") {
    stop(\"Cannot find Shiny app. Try re-installing `SWDSFODR`.\", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = \"normal\")
}
"
writeLines(shiny_launcher_R, "SWDSFODR/R/shinyLauncher.R")
