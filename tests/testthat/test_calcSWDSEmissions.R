# test_calcSWDSEmissions.R

library(testthat)
library(SWDSFODR)

test_that("calcSWDSEmissionsYearly works with small example", {
  waste_data <- data.frame(
    year = rep(1:2, each = 2),
    waste_type = rep(c("food", "paper"), 2),
    mass_tonnes = c(100, 50, 120, 60)
  )
  docj <- c(food = 0.15, paper = 0.40)
  kj   <- c(food = 0.06, paper = 0.04)
  phi  <- 1
  f    <- 0
  gwp  <- 28
  ox   <- 0.1
  frac <- 0.5
  mcf  <- 1
  val <- calcSWDSEmissionsYearly(
    W = waste_data,
    DOCj = docj,
    k_j = kj,
    phi_y = phi,
    f_y = f,
    GWP_CH4 = gwp,
    OX = ox,
    F = 0.5,
    DOCf_y = frac,
    MCF_y = mcf,
    year_target = 2
  )
  expect_true(val > 0)
  expect_type(val, "double")
})

test_that("calcSWDSEmissionsSimplified returns numeric", {
  waste_data <- data.frame(
    year = 1:5,
    mass_tonnes = c(100, 120, 140, 160, 180)
  )
  val <- calcSWDSEmissionsSimplified(
    W = waste_data,
    phi_y = 1, f_y = 0, GWP_CH4 = 28,
    climate_zone = "tropical_wet",
    year_target = 5
  )
  expect_true(is.numeric(val))
  expect_true(val > 0)
})

test_that("calcSWDSEmissionsYearly works with multi-region example", {
  waste_data <- data.frame(
    year = rep(1:2, each = 4),
    region = rep(c("region1", "region2"), each = 2, times = 2),
    waste_type = rep(c("food", "paper"), 4),
    mass_tonnes = c(100, 50, 80, 40, 120, 60, 90, 45)
  )
  docj <- c(food = 0.15, paper = 0.40)
  kj   <- c(food = 0.06, paper = 0.04)
  
  phi_values <- c(region1 = 0.85, region2 = 0.75)
  f_values <- c(region1 = 0.2, region2 = 0)
  
  results_by_region <- calcSWDSEmissionsYearly(
    W = waste_data,
    DOCj = docj,
    k_j = kj,
    phi_y = phi_values,
    f_y = f_values,
    GWP_CH4 = 28,
    OX = 0.1,
    F = 0.5,
    DOCf_y = 0.5,
    MCF_y = 1,
    year_target = 2,
    by_region = TRUE
  )
  
  # Check that we have a result for each region
  expect_equal(length(results_by_region), 2)
  expect_true(all(c("region1", "region2") %in% names(results_by_region)))
  
  # Check that both values are positive
  expect_true(all(results_by_region > 0))
  
  # Check that region1 (with capture) has lower emissions than region2
  expect_true(results_by_region["region1"] < results_by_region["region2"])
  
  # Test combined result
  combined_result <- calcSWDSEmissionsYearly(
    W = waste_data,
    DOCj = docj,
    k_j = kj,
    phi_y = phi_values,
    f_y = f_values,
    GWP_CH4 = 28,
    OX = 0.1,
    F = 0.5,
    DOCf_y = 0.5,
    MCF_y = 1,
    year_target = 2,
    by_region = FALSE
  )
  
  # Combined result should equal sum of individual results
  expect_equal(combined_result, sum(results_by_region))
})

test_that("calcSWDSEmissionsSimplified works with multi-region example", {
  waste_data <- data.frame(
    year = rep(1:5, 2),
    region = rep(c("region1", "region2"), each = 5),
    mass_tonnes = c(100, 120, 140, 160, 180, 200, 220, 240, 260, 280)
  )
  
  climate_zones <- c(region1 = "tropical_wet", region2 = "tropical_dry")
  
  results_by_region <- calcSWDSEmissionsSimplified(
    W = waste_data,
    phi_y = 1,
    f_y = 0,
    GWP_CH4 = 28,
    climate_zone = climate_zones,
    year_target = 5,
    by_region = TRUE
  )
  
  # Check that we have a result for each region
  expect_equal(length(results_by_region), 2)
  expect_true(all(c("region1", "region2") %in% names(results_by_region)))
  
  # Check that both values are positive
  expect_true(all(results_by_region > 0))
  
  # Check that tropical_wet (region1) has higher emissions than tropical_dry (region2)
  # despite having lower waste amounts, due to climate factors
  expect_true(results_by_region["region1"] > results_by_region["region2"])
})

test_that("calcSWDSEmissionsMonthly works with multi-region example", {
  waste_data <- data.frame(
    month = rep(1:6, each = 4),
    region = rep(c("region1", "region2"), each = 2, times = 6),
    waste_type = rep(c("food", "paper"), 12),
    mass_tonnes = c(
      10, 5, 8, 4,
      11, 5.5, 8.8, 4.4,
      12, 6, 9.6, 4.8,
      13, 6.5, 10.4, 5.2,
      14, 7, 11.2, 5.6,
      15, 7.5, 12, 6
    )
  )
  docj <- c(food = 0.15, paper = 0.40)
  kj   <- c(food = 0.06, paper = 0.04)
  
  results_by_region <- calcSWDSEmissionsMonthly(
    W = waste_data,
    DOCj = docj,
    k_j = kj,
    phi_y = 1,
    f_y = 0,
    GWP_CH4 = 28,
    OX = 0.1,
    F = 0.5,
    DOCf_m = 0.5,
    MCF_y = 1,
    month_target = 6,
    by_region = TRUE
  )
  
  # Check that we have a result for each region
  expect_equal(length(results_by_region), 2)
  expect_true(all(c("region1", "region2") %in% names(results_by_region)))
  
  # Check that both values are positive
  expect_true(all(results_by_region > 0))
})