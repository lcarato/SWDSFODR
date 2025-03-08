# simplifiedApproaches.R
# Implements the simplified approaches from the appendix of Tool 04 (v08.1),
# specifically "No waste composition monitoring" and "Reduced waste composition monitoring".
# Enhanced with multi-region support.

# -----------------------------------------------------------
#' Calculate SWDS CH4 Emissions (Simplified - No Composition Monitoring)
#'
#' @description
#'   Implements the approach with no composition monitoring, using a table of default factors
#'   from the Appendix of Tool 04 (v08.1). Enhanced with region-specific parameter support.
#'
#' @param W A data.frame giving the total annual waste disposal, with columns:
#'    year, mass_tonnes, and optionally region.
#' @param phi_y Model correction factor. Can be a single value or a named
#'        vector with region names.
#' @param f_y Fraction of CH4 captured and destroyed. Can be a single value or a 
#'        named vector with region names.
#' @param GWP_CH4 Global Warming Potential of CH4.
#' @param climate_zone A string for the climate zone or a named vector with region names.
#'    Values should be in c("tropical_wet","tropical_dry","boreal_temperate_wet",
#'    "boreal_temperate_dry") used to pick the correct column from the default table.
#' @param year_target The year for which we want CH4 generation. Can be a single value
#'    or a named vector with region names.
#' @param default_table (Optional) A data.frame or matrix of default factors by climate zone.
#' @param by_region Logical, if TRUE returns results by region, otherwise returns total sum.
#'
#' @return If by_region is TRUE, a named vector of estimated CH4 emissions (in t CO2-equivalent) 
#'         for each region. Otherwise, returns a single value for all regions combined.
#'
#' @references
#'   Tool 04 (v08.1) Appendix, "No waste composition monitoring".
#'
#' @export
calcSWDSEmissionsSimplified <- function(W,
                                      phi_y = 1,
                                      f_y = 0,
                                      GWP_CH4 = 28,
                                      climate_zone = "tropical_wet",
                                      year_target = max(W$year),
                                      default_table = NULL,
                                      by_region = FALSE) {
  
  if(is.null(default_table)) {
    default_table <- data.frame(
      x = 1:21,
      tropical_wet = c(0.005800,0.004212,0.003093,0.002275,0.001657,0.001198,0.000867,
                       0.000635,0.000474,0.000362,0.000284,0.000228,0.000189,0.000160,
                       0.000138,0.000122,0.000109,0.000098,0.000090,0.000082,0.000076),
      tropical_dry = c(0.001856,0.001724,0.001601,0.001487,0.001381,0.001281,0.001189,
                       0.001103,0.001024,0.000950,0.000881,0.000817,0.000757,0.000702,
                       0.000651,0.000603,0.000559,0.000518,0.000480,0.000445,0.000413),
      boreal_temperate_wet = c(0.003382,0.002913,0.002511,0.002163,0.001861,0.001599,
                               0.001371,0.001174,0.001004,0.000859,0.000734,0.000629,
                               0.000539,0.000463,0.000399,0.000344,0.000298,0.000259,
                               0.000226,0.000197,0.000173),
      boreal_temperate_dry = c(0.001399,0.001325,0.001254,0.001188,0.001125,0.001065,
                               0.001008,0.000954,0.000904,0.000855,0.000810,0.000766,
                               0.000725,0.000687,0.000650,0.000615,0.000582,0.000551,
                               0.000521,0.000493,0.000467)
    )
  }
  
  # Check if W has a region column, if not, add a default region
  if(!"region" %in% names(W)) {
    W$region <- "default"
  }
  
  # Get unique regions
  regions <- unique(W$region)
  
  # Initialize results vector
  results <- numeric(length(regions))
  names(results) <- regions
  
  # Process each region
  for(region in regions) {
    # Filter data for this region
    W_region <- W[W$region == region, , drop = FALSE]
    
    # Get region-specific parameters
    phi_region <- if(length(phi_y) > 1 && !is.null(names(phi_y)) && region %in% names(phi_y)) phi_y[region] else phi_y[1]
    f_region <- if(length(f_y) > 1 && !is.null(names(f_y)) && region %in% names(f_y)) f_y[region] else f_y[1]
    climate_zone_region <- if(length(climate_zone) > 1 && !is.null(names(climate_zone)) && region %in% names(climate_zone)) climate_zone[region] else climate_zone[1]
    year_target_region <- if(length(year_target) > 1 && !is.null(names(year_target)) && region %in% names(year_target)) year_target[region] else year_target[1]
    
    outside_factor <- phi_region * (1 - f_region) * GWP_CH4
    
    # Filter W for year <= year_target for this region
    W_use <- W_region[W_region$year <= year_target_region, , drop = FALSE]
    
    partial_sum <- 0
    for(r in seq_len(nrow(W_use))) {
      x_val <- W_use$year[r]
      mass <- W_use$mass_tonnes[r]
      row_id <- if(x_val > 21) 21 else x_val
      factor_for_x <- default_table[row_id, climate_zone_region]
      partial_sum <- partial_sum + factor_for_x * mass
    }
    
    result_tco2e <- outside_factor * partial_sum
    # Ensure emissions are non-negative
    result_tco2e <- max(result_tco2e, 0)
    results[region] <- result_tco2e
  }
  
  # Return results
  if(by_region) {
    return(results)
  } else {
    return(sum(results))
  }
}