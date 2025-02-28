# shinyLauncher.R
# Provides a function to run the Shiny App located in inst/shiny-app

#' Launch the SWDSFODR Shiny Application
#'
#' @description
#'   Opens a Shiny GUI where the user can input data for the FOD model, 
#'   select an approach (annual / monthly / simplified), and see interactive charts/tables.
#'
#' @export
runSWDSFODApp <- function() {
  appDir <- system.file("shiny-app", package = "SWDSFODR")
  if (appDir == "") {
    stop("Cannot find Shiny app. Try re-installing `SWDSFODR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
