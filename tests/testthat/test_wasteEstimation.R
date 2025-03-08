# test_wasteEstimation.R

library(testthat)
library(SWDSFODR)

test_that("estimateMSWGeneration correctly estimates waste", {
  # Test simple model
  demographic_data <- data.frame(
    region = c("region1", "region2"),
    population = c(100000, 200000)
  )
  
  simple_result <- estimateMSWGeneration(
    population = demographic_data$population,
    regions = demographic_data$region,
    model = "simple"
  )
  
  expect_equal(nrow(simple_result), 2)
  expect_true(all(c("region", "estimated_waste_tonnes") %in% names(simple_result)))
  
  # Region2 should have twice the waste of region1 with simple model
  expect_equal(simple_result$estimated_waste_tonnes[2] / simple_result$estimated_waste_tonnes[1], 2)
})

test_that("distributeMSWByType correctly splits waste", {
  msw_data <- data.frame(
    year = c(2020, 2020),
    region = c("urban", "rural"),
    estimated_waste_tonnes = c(10000, 5000)
  )
  
  waste_composition <- distributeMSWByType(msw_data)
  
  expect_true("waste_type" %in% names(waste_composition))
  
  # Check that the total waste matches the input
  urban_total <- sum(waste_composition$mass_tonnes[waste_composition$region == "urban"])
  rural_total <- sum(waste_composition$mass_tonnes[waste_composition$region == "rural"])
  
  expect_equal(urban_total, 10000)
  expect_equal(rural_total, 5000)
})

test_that("projectWasteGeneration correctly projects waste", {
  historical_data <- data.frame(
    year = c(2020, 2021, 2022),
    mass_tonnes = c(1000, 1100, 1210)
  )
  
  projected_data <- projectWasteGeneration(
    historical_data = historical_data,
    projection_years = 2023:2025,
    growth_rate = 0.1,
    growth_model = "exponential"
  )
  
  expect_equal(nrow(projected_data), 6)  # 3 historical + 3 projected
  
  # Check that 2023 is approximately 10% more than 2022
  expect_equal(
    projected_data$mass_tonnes[projected_data$year == 2023] / projected_data$mass_tonnes[projected_data$year == 2022],
    1.1, 
    tolerance = 0.01
  )
})