# calcSWDSEmissions.R
# Main functions for calculating methane emissions from SWDS
# according to the equations given in Tool 04 (v08.1).

# -----------------------------------------------------------
#' Calculate SWDS CH4 Emissions (Annual Model)
#'
#' @description
#'   Implements Equation (1) from Tool 04 (v08.1) for a yearly approach.
#'
#' @param W A data.frame or list giving the amounts of waste disposed (W_j,x).
#'        Must include columns/entries for each waste type j, by year x.
#'        For example, a data frame with columns: year, waste_type, mass_tonnes.
#' @param DOCj Vector or named list of degradable organic carbon fraction(s)
#'        for each waste type j. If your data has multiple waste types, ensure
#'        names(DOCj) match levels of waste_type in your data.
#' @param k_j Vector or named list of decay rates for each waste type j (1/yr).
#' @param phi_y Model correction factor for year y (phi_y).
#' @param f_y Fraction of CH4 captured and destroyed in year y (f_y).
#' @param GWP_CH4 Global Warming Potential of CH4.
#' @param OX Oxidation factor.
#' @param F Fraction of CH4 in landfill gas.
#' @param DOCf_y Fraction of degradable organic carbon that decomposes (DOCf).
#' @param MCF_y Methane correction factor for year y.
#' @param year_target The year y for which we want to calculate CH4 generation.
#'
#' @return Estimated CH4 emissions (in t CO2-equivalent) for that target year.
#'
#' @references
#'   Tool 04: Emissions from solid waste disposal sites (Version 08.1).
#' @examples
#' # Suppose we have 2 waste types: "food" and "paper"
#' # We have disposal data for years 1 to 5 (tonnes).
#' waste_data <- data.frame(
#'   year = rep(1:5, each=2),
#'   waste_type = rep(c("food","paper"), 5),
#'   mass_tonnes = c(100,50,120,60,140,70,160,80,180,90)
#' )
#'
#' # Parameter assumptions:
#' docj  <- c(food=0.15, paper=0.40)
#' kj    <- c(food=0.06, paper=0.04)
#' phi   <- 1    # For project or leakage, can be 1
#' f     <- 0.0  # No methane recovery
#' gwp   <- 28   # AR5 default for methane
#' ox    <- 0.1
#' frac  <- 0.5
#' mcf   <- 1.0
#' # Calculate for year = 5
#' calcSWDSEmissionsYearly(
#'   W = waste_data, DOCj = docj, k_j = kj,
#'   phi_y = phi, f_y = f, GWP_CH4 = gwp,
#'   OX = ox, F = 0.5, DOCf_y = frac,
#'   MCF_y = mcf, year_target = 5
#' )
#'
#' @export
calcSWDSEmissionsYearly <- function(W,
                                    DOCj,
                                    k_j,
                                    phi_y = 1,
                                    f_y   = 0,
                                    GWP_CH4 = 28,
                                    OX = 0.1,
                                    F  = 0.5,
                                    DOCf_y = 0.5,
                                    MCF_y  = 1,
                                    year_target = 1) {
  # Filter data for years <= year_target
  W_use <- W[W$year <= year_target, , drop=FALSE]

  # Equation (1) factor outside sums:
  outside_factor <- phi_y * (1 - f_y) * GWP_CH4 * (1 - OX) * (16/12) * F * DOCf_y * MCF_y

  # Check that W has columns: year, waste_type, mass_tonnes
  if(!all(c("year","waste_type","mass_tonnes") %in% names(W_use))) {
    stop("Input data frame W must have columns: year, waste_type, mass_tonnes")
  }

  # Sum over each row
  partial_sum <- 0
  for(r in seq_len(nrow(W_use))) {
    row_year  <- W_use$year[r]
    row_type  <- as.character(W_use$waste_type[r])
    row_mass  <- W_use$mass_tonnes[r]
    if(!row_type %in% names(DOCj)) stop("DOCj does not have an entry for waste type: ", row_type)
    if(!row_type %in% names(k_j))   stop("k_j does not have an entry for waste type: ", row_type)

    d_j   <- DOCj[[row_type]]
    k_val <- k_j[[row_type]]
    age   <- year_target - row_year
    if(age < 0) age <- 0

    # Summation term = W_j,x * DOC_j * exp(-k_j*(y-x)) * (1 - exp(-k_j))
    term <- row_mass * d_j * exp(-k_val * age) * (1 - exp(-k_val))
    partial_sum <- partial_sum + term
  }

  result_tco2e <- outside_factor * partial_sum
  # Ensure emissions are non-negative and cumulative emissions never decline
  result_tco2e <- pmax(result_tco2e, 0)
  return(result_tco2e)
}

# -----------------------------------------------------------
#' Calculate SWDS CH4 Emissions (Monthly Model)
#'
#' @description
#'   Implements Equation (2) from Tool 04 (v08.1) for a monthly approach.
#'
#' @param W A data.frame or list giving the amounts of waste disposed (W_j,i)
#'        Must include columns: month, waste_type, mass_tonnes.
#'        month is a numeric from 1..N.
#' @param DOCj Vector or named list of degradable organic carbon fraction(s).
#' @param k_j Vector or named list of decay rates for each waste type j (1/yr).
#' @param phi_y Model correction factor (phi_y).
#' @param f_y Fraction of CH4 captured and destroyed (f_y).
#' @param GWP_CH4 Global Warming Potential of CH4.
#' @param OX Oxidation factor.
#' @param F Fraction of CH4 in landfill gas.
#' @param DOCf_m Fraction of degradable organic carbon that decomposes (DOCf).
#' @param MCF_y Methane correction factor.
#' @param month_target The month m for which we want to calculate CH4 generation.
#'
#' @return Estimated CH4 emissions (in t CO2-equivalent) for that month.
#'
#' @references
#'   Tool 04: Emissions from solid waste disposal sites (Version 08.1).
#'
#' @examples
#' # Example monthly data:
#' monthly_data <- data.frame(
#'   month = rep(1:6, each=2),
#'   waste_type = rep(c("food","paper"), 6),
#'   mass_tonnes = c(10,5,12,6,14,7,16,8,18,9,20,10)
#' )
#' docj  <- c(food=0.15, paper=0.40)
#' kj    <- c(food=0.06, paper=0.04)
#' res <- calcSWDSEmissionsMonthly(
#'   W=monthly_data, DOCj=docj, k_j=kj,
#'   phi_y=1, f_y=0, GWP_CH4=28, OX=0.1, F=0.5,
#'   DOCf_m=0.5, MCF_y=1, month_target=6
#' )
#' @export
calcSWDSEmissionsMonthly <- function(W,
                                     DOCj,
                                     k_j,
                                     phi_y = 1,
                                     f_y   = 0,
                                     GWP_CH4 = 28,
                                     OX = 0.1,
                                     F  = 0.5,
                                     DOCf_m = 0.5,
                                     MCF_y  = 1,
                                     month_target = 1) {
  # Filter data for months <= month_target
  W_use <- W[W$month <= month_target, , drop=FALSE]
  outside_factor <- phi_y * (1 - f_y) * GWP_CH4 * (1 - OX) * (16/12) * F * DOCf_m * MCF_y

  partial_sum <- 0
  for(r in seq_len(nrow(W_use))) {
    row_month <- W_use$month[r]
    row_type  <- as.character(W_use$waste_type[r])
    row_mass  <- W_use$mass_tonnes[r]

    if(!row_type %in% names(DOCj)) stop("DOCj does not have an entry for waste type: ", row_type)
    if(!row_type %in% names(k_j))   stop("k_j does not have an entry for waste type: ", row_type)

    d_j   <- DOCj[[row_type]]
    k_val <- k_j[[row_type]]
    age_m <- month_target - row_month
    if(age_m < 0) age_m <- 0

    term <- row_mass * d_j * exp(-k_val * (age_m / 12)) * (1 - exp(-k_val / 12))
    partial_sum <- partial_sum + term
  }

  result_tco2e <- outside_factor * partial_sum
  return(result_tco2e)
}
