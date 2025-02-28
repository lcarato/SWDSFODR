simplifiedApproaches_R <- '
# simplifiedApproaches.R
# Implements the simplified approaches from the appendix of Tool 04 (v08.1),
# specifically "No waste composition monitoring" and "Reduced waste composition monitoring".

# -----------------------------------------------------------
# Calculate SWDS CH4 Emissions (Simplified - No Composition Monitoring)
#
# @description
#   Implements the approach with no composition monitoring, using table of default_x
#   from the Appendix of Tool 04 (v08.1).
#
# @param W A data.frame or list giving the total annual waste disposal, e.g. columns:
#    year, mass_tonnes. We assume all is MSW with no composition breakdown.
# @param phi_y Model correction factor
# @param f_y Fraction of CH4 captured and destroyed
# @param GWP_CH4 Global Warming Potential of CH4
# @param default_table A data.frame or matrix of default factors by climate zone
#    (like the one in the official tool). The user can provide or use a built-in set.
# @param climate_zone A string in c("tropical_wet","tropical_dry","boreal_temperate_wet",
#    "boreal_temperate_dry") used to pick the correct column from default_table.
# @param year_target The year for which we want CH4 generation (the sum of each x up to that year).
#
# @return t CO2-equivalent methane emissions for that year.
#
# @references
#   Tool 04 (v08.1) Appendix, "No waste composition monitoring".
#
# @examples
# # Suppose we only track total MSW disposal (tonnes) each year 1..5
# waste_data <- data.frame(
#   year = 1:5,
#   mass_tonnes = c(100,120,140,160,180)
# )
# # We pick a climate zone, e.g. "tropical_wet"
# res <- calcSWDSEmissionsSimplified(
#   W = waste_data, phi_y=1, f_y=0, GWP_CH4=28,
#   climate_zone="tropical_wet", year_target=5
# )
# @export
calcSWDSEmissionsSimplified <- function(W,
                                        phi_y=1,
                                        f_y=0,
                                        GWP_CH4=28,
                                        climate_zone="tropical_wet",
                                        year_target=max(W$year),
                                        default_table=NULL) {

  # If default_table not provided, we embed the one from the tool's Appendix:
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

  # Outer factor: phi_y * (1 - f_y) * GWP_CH4
  # from eq (14) in the Appendix
  outside_factor <- phi_y * (1 - f_y) * GWP_CH4

  # Filter W for year <= year_target
  W_use <- W[W$year <= year_target, , drop=FALSE]
  # For each year x, we do Default_x * W_x
  # Then sum over x
  partial_sum <- 0
  for(r in seq_len(nrow(W_use))) {
    x_val <- W_use$year[r]
    mass  <- W_use$mass_tonnes[r]
    if(x_val > 21) {
      # The table only goes to x=21, so for x>21 the default factor might be minimal or 0
      # The tool table stops at year 21. We assume the default is the last row if needed.
      row_id <- 21
    } else {
      row_id <- x_val
    }
    factor_for_x <- default_table[row_id, climate_zone]
    partial_sum <- partial_sum + factor_for_x * mass
  }

  result_tco2e <- outside_factor * partial_sum
  return(result_tco2e)
}
'
writeLines(simplifiedApproaches_R, "SWDSFOD/R/simplifiedApproaches.R")
