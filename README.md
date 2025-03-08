# SWDSFODR

## Overview

SWDSFODR is an R package that implements CDM TOOL 04 (V08.1) - GHG Emissions from solid waste disposal sites. The package provides functions to calculate methane emissions from waste disposal using the First Order Decay (FOD) model.

## New in Version 1.1.0

Version 1.1.0 adds several enhancements while maintaining full backward compatibility:

1. **Multi-region support**: Calculate emissions for multiple regions with region-specific parameters
2. **Waste estimation module**: Estimate waste generation based on demographic and economic data
3. **Projection capabilities**: Project future waste generation based on various growth models

## Installation

```r
# Install directly from GitHub
devtools::install_github("YOUR-USERNAME/SWDSFODR")
```

## Usage Examples

### Basic Usage

```r
library(SWDSFODR)

# Prepare waste data
waste_data <- data.frame(
  year = rep(1:5, each = 2),
  waste_type = rep(c("food", "paper"), 5),
  mass_tonnes = c(100, 50, 120, 60, 140, 70, 160, 80, 180, 90)
)

# Set parameters
docj <- c(food = 0.15, paper = 0.40)
kj <- c(food = 0.06, paper = 0.04)

# Calculate emissions
emissions <- calcSWDSEmissionsYearly(
  W = waste_data, 
  DOCj = docj, 
  k_j = kj,
  phi_y = 1, 
  f_y = 0, 
  GWP_CH4 = 28,
  OX = 0.1, 
  F = 0.5, 
  DOCf_y = 0.5,
  MCF_y = 1, 
  year_target = 5
)

print(paste("Total emissions:", emissions, "t CO2e"))
```

### Multi-Region Support

```r
library(SWDSFODR)

# Multi-region waste data
waste_data <- data.frame(
  year = rep(1:5, each = 4),
  region = rep(c("urban", "rural"), each = 2, times = 5),
  waste_type = rep(c("food", "paper"), 10),
  mass_tonnes = c(
    100, 50, 80, 40,
    120, 60, 90, 45,
    140, 70, 100, 50,
    160, 80, 110, 55,
    180, 90, 120, 60
  )
)

# Basic parameters
docj <- c(food = 0.15, paper = 0.40)
kj <- c(food = 0.06, paper = 0.04)

# Region-specific parameters
phi_values <- c(urban = 0.85, rural = 0.75)
f_values <- c(urban = 0.2, rural = 0)  # Urban has some CH4 capture, rural has none
MCF_values <- c(urban = 1.0, rural = 0.4)  # Urban is managed, rural is unmanaged-shallow

# Calculate emissions by region
emissions_by_region <- calcSWDSEmissionsYearly(
  W = waste_data,
  DOCj = docj,
  k_j = kj,
  phi_y = phi_values,
  f_y = f_values,
  MCF_y = MCF_values,
  year_target = 5,
  by_region = TRUE
)

print("Emissions by region (t CO2e):")
print(emissions_by_region)

# Get total emissions
total_emissions <- sum(emissions_by_region)
print(paste("Total emissions:", total_emissions, "t CO2e"))
```

### Waste Estimation Module

```r
library(SWDSFODR)

# Demographic data
demographic_data <- data.frame(
  region = c("urban", "rural"),
  population = c(500000, 300000),
  gdp_per_capita = c(15000, 8000),
  urban_pct = c(90, 35)
)

# Estimate waste generation
waste_estimates <- estimateMSWGeneration(
  population = demographic_data$population,
  gdp_per_capita = demographic_data$gdp_per_capita,
  urban_pct = demographic_data$urban_pct,
  regions = demographic_data$region,
  model = "comprehensive"
)

# Distribute into waste types
waste_composition <- distributeMSWByType(waste_estimates)

# Calculate emissions using the estimated waste data
emissions <- calcSWDSEmissionsYearly(
  W = waste_composition,
  DOCj = c(food = 0.15, paper = 0.40, wood = 0.43, textiles = 0.24, garden = 0.20, other = 0.15),
  k_j = c(food = 0.06, paper = 0.04, wood = 0.02, textiles = 0.04, garden = 0.05, other = 0.05),
  year_target = max(waste_composition$year),
  by_region = TRUE
)

print("Estimated emissions by region (t CO2e):")
print(emissions)
```

### Shiny Application

The package includes a Shiny application that can be run in two modes:

```r
# Run the original interface
runSWDSFODApp()

# Run the enhanced interface with multi-region support and waste estimation
runSWDSFODApp(enhanced = TRUE)
```

## Data Format

### Multi-Region Waste Data Format

For annual approach:
```
year,waste_type,mass_tonnes,region
2020,food,1000,region1
2020,paper,500,region1
2021,food,1100,region1
2020,food,800,region2
...
```

For monthly approach:
```
month,waste_type,mass_tonnes,region
1,food,100,region1
1,paper,50,region1
2,food,110,region1
1,food,80,region2
...
```

For simplified approach:
```
year,mass_tonnes,region
2020,1800,region1
2021,1940,region1
2020,1200,region2
...
```

### Demographic Data Format (for Waste Estimation)

```
region,population,gdp_per_capita,urban_pct
region1,500000,15000,75
region2,300000,10000,60
...
```

## License

This package is released under the GPL-3 license.