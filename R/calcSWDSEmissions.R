dir.create("SWDSFODR", showWarnings=FALSE)
dir.create("SWDSFODR/R", showWarnings=FALSE)
# calcSWDSEmissions.R - implementation of yearly and monthly models
calc_emissions_R <- "#' Calculate SWDS CH4 Emissions (Annual Model)
#'
#' @description
#'   Implements Equation (1) from Tool 04 (v08.1) to estimate annual CH4 emissions.
#'
#' @param W A data frame with columns year, waste_type, mass_tonnes
#' @param DOCj Named numeric for degradable organic carbon fraction(s)
#' @param k_j  Named numeric for decay rate(s)
#' @param phi_y Model correction factor
#' @param f_y Fraction of CH4 captured
#' @param GWP_CH4 Global warming potential of CH4
#' @param OX Oxidation factor
#' @param F Fraction of CH4 in landfill gas
#' @param DOCf_y Fraction of degradable organic carbon that decomposes
#' @param MCF_y Methane correction factor
#' @param year_target The year for which to calculate CH4 generation
#'
#' @return Estimated CH4 emissions in t CO2e
#' @export
calcSWDSEmissionsYearly <- function(W,
                                    DOCj,
                                    k_j,
                                    phi_y = 1,
                                    f_y = 0,
                                    GWP_CH4 = 28,
                                    OX = 0.1,
                                    F = 0.5,
                                    DOCf_y = 0.5,
                                    MCF_y = 1,
                                    year_target = max(W$year)) {
  # Check input consistency
  waste_types <- unique(W$waste_type)
  if(!all(waste_types %in% names(DOCj)) || !all(waste_types %in% names(k_j))) {
    stop(\"All waste types in data must have corresponding DOCj and k_j values\")
  }
  
  # Filter data to years <= target year
  W_filtered <- W[W$year <= year_target, ]
  
  # Initialize total CH4 generation
  total_ch4 <- 0
  
  # Calculate for each waste type j
  for (j in waste_types) {
    # Filter data for this waste type
    W_j <- W_filtered[W_filtered$waste_type == j, ]
    
    # Get parameters for waste type j
    doc_j <- DOCj[j]
    k_j_val <- k_j[j]
    
    # Calculate sum over x (years of waste deposits)
    ch4_j <- 0
    for (x in 1:nrow(W_j)) {
      year_x <- W_j$year[x]
      waste_x <- W_j$mass_tonnes[x]
      
      # Time since deposition (y - x) in years
      time_diff <- year_target - year_x
      
      # Calculate the term inside the sum
      decay_term <- exp(-k_j_val * time_diff) * (1 - exp(-k_j_val))
      
      # Accumulate
      ch4_j <- ch4_j + waste_x * doc_j * decay_term
    }
    
    # Add this waste type's contribution to total
    total_ch4 <- total_ch4 + ch4_j
  }
  
  # Apply remaining factors from equation (1)
  result <- phi_y * (1 - f_y) * F * MCF_y * DOCf_y * (16/12) * total_ch4 * (1 - OX) * GWP_CH4
  
  return(result)
}

#' Calculate SWDS CH4 Emissions (Monthly Model)
#'
#' @description
#'   Implements the monthly version of the FOD model to estimate CH4 emissions
#'   with higher temporal resolution.
#'
#' @param W A data frame with columns month, waste_type, mass_tonnes
#' @param DOCj Named numeric for degradable organic carbon fraction(s)
#' @param k_j  Named numeric for decay rate(s)
#' @param phi_y Model correction factor
#' @param f_y Fraction of CH4 captured
#' @param GWP_CH4 Global warming potential of CH4
#' @param OX Oxidation factor
#' @param F Fraction of CH4 in landfill gas
#' @param DOCf_m Fraction of degradable organic carbon that decomposes
#' @param MCF_y Methane correction factor
#' @param month_target The month for which to calculate CH4 generation (integer from 1...)
#'
#' @return Estimated CH4 emissions in t CO2e
#' @export
calcSWDSEmissionsMonthly <- function(W,
                                      DOCj,
                                      k_j,
                                      phi_y = 1,
                                      f_y = 0,
                                      GWP_CH4 = 28,
                                      OX = 0.1,
                                      F = 0.5,
                                      DOCf_m = 0.5,
                                      MCF_y = 1,
                                      month_target = max(W$month)) {
  # Check input consistency
  waste_types <- unique(W$waste_type)
  if(!all(waste_types %in% names(DOCj)) || !all(waste_types %in% names(k_j))) {
    stop(\"All waste types in data must have corresponding DOCj and k_j values\")
  }
  
  # Filter data to months <= target month
  W_filtered <- W[W$month <= month_target, ]
  
  # Convert monthly decay rates (k values need to be divided by 12)
  k_j_monthly <- k_j / 12
  
  # Initialize total CH4 generation
  total_ch4 <- 0
  
  # Calculate for each waste type j
  for (j in waste_types) {
    # Filter data for this waste type
    W_j <- W_filtered[W_filtered$waste_type == j, ]
    
    # Get parameters for waste type j
    doc_j <- DOCj[j]
    k_j_val <- k_j_monthly[j]
    
    # Calculate sum over m (months of waste deposits)
    ch4_j <- 0
    for (x in 1:nrow(W_j)) {
      month_x <- W_j$month[x]
      waste_x <- W_j$mass_tonnes[x]
      
      # Time since deposition (month_target - month_x) in months
      time_diff <- month_target - month_x
      
      # Calculate the term inside the sum
      decay_term <- exp(-k_j_val * time_diff) * (1 - exp(-k_j_val))
      
      # Accumulate
      ch4_j <- ch4_j + waste_x * doc_j * decay_term
    }
    
    # Add this waste type's contribution to total
    total_ch4 <- total_ch4 + ch4_j
  }
  
  # Apply remaining factors from equation (1), adapted for monthly calculations
  result <- phi_y * (1 - f_y) * F * MCF_y * DOCf_m * (16/12) * total_ch4 * (1 - OX) * GWP_CH4
  
  return(result)
}
"
writeLines(calc_emissions_R, "SWDSFODR/R/calcSWDSEmissions.R")
