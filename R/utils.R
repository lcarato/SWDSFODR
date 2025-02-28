# utils.R
utils_R <- "#' Utility functions for the SWDSFODR package
#'
#' @description
#'   Helper functions for parameter validation and data processing.
#'
#' @keywords internal

# Check climate zone validity
validate_climate_zone <- function(climate_zone) {
  valid_zones <- c(\"tropical_wet\", \"tropical_dry\", \"boreal_temperate_wet\", \"boreal_temperate_dry\")
  if(!climate_zone %in% valid_zones) {
    stop(\"Invalid climate zone. Must be one of: \", paste(valid_zones, collapse=\", \"))
  }
  return(TRUE)
}

# Check if waste data has required columns
validate_waste_data <- function(W, approach = \"annual\") {
  if(approach == \"annual\") {
    required_cols <- c(\"year\", \"waste_type\", \"mass_tonnes\")
  } else if(approach == \"monthly\") {
    required_cols <- c(\"month\", \"waste_type\", \"mass_tonnes\")
  } else if(approach == \"simplified\") {
    required_cols <- c(\"year\", \"mass_tonnes\")
  } else {
    stop(\"Invalid approach. Must be one of: annual, monthly, simplified\")
  }
  
  missing_cols <- setdiff(required_cols, names(W))
  if(length(missing_cols) > 0) {
    stop(\"Missing required columns: \", paste(missing_cols, collapse=\", \"))
  }
  return(TRUE)
}
"
writeLines(utils_R, "SWDSFODR/R/utils.R")
