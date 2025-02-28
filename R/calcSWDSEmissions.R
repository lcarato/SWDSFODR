# calcSWDSEmissions.R
# Calculate SWDS CH4 Emissions (Annual Model)
#
# @description
#   Implements Equation (1) from Tool 04 (v08.1) to estimate annual CH4 emissions.
#
# @param W A data frame with columns year, waste_type, mass_tonnes
# @param DOCj Named numeric for degradable organic carbon fraction(s)
# @param k_j  Named numeric for decay rate(s)
# @param phi_y Model correction factor
# @param f_y Fraction of CH4 captured
# @param GWP_CH4 Global warming potential of CH4
# @param OX Oxidation factor
# @param F Fraction of CH4 in landfill gas
# @param DOCf_y Fraction of degradable organic carbon that decomposes
# @param MCF_y Methane correction factor
# @param year_target The year for which to calculate CH4 generation
#
# @return Estimated CH4 emissions in t CO2e
#
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
                                    year_target = 1) {
  # ...function body here...
  0  # placeholder
}
