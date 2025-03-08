# shinyLauncher.R
# Provides a function to run the Shiny App

#' Launch the SWDSFODR Shiny Application
#'
#' @description
#'   Opens a Shiny GUI where the user can input data for the FOD model, 
#'   select an approach (annual / monthly / simplified), and see interactive charts/tables.
#'   The enhanced version includes multi-region support and waste estimation capabilities.
#'
#' @param enhanced Logical, if TRUE runs the enhanced version with multi-region support
#'                and waste estimation capabilities. If FALSE runs the original version.
#'
#' @export
runSWDSFODApp <- function(enhanced = FALSE) {
  if(enhanced) {
    appDir <- system.file("shiny-app-enhanced", package = "SWDSFODR")
    if (appDir == "") {
      stop("Cannot find enhanced Shiny app. Try re-installing `SWDSFODR`.", call. = FALSE)
    }
  } else {
    appDir <- system.file("shiny-app", package = "SWDSFODR")
    if (appDir == "") {
      stop("Cannot find Shiny app. Try re-installing `SWDSFODR`.", call. = FALSE)
    }
  }
  shiny::runApp(appDir, display.mode = "normal")
}