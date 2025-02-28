dir.create("SWDSFOD/tests", showWarnings=FALSE)
dir.create("SWDSFOD/tests/testthat", showWarnings=FALSE)

test_calcSWDSEmissions_R <- '
# test_calcSWDSEmissions.R

library(testthat)
library(SWDSFOD)

test_that("calcSWDSEmissionsYearly works with small example", {
  waste_data <- data.frame(
    year = rep(1:2, each=2),
    waste_type = rep(c("food","paper"), 2),
    mass_tonnes = c(100,50,120,60)
  )
  docj <- c(food=0.15, paper=0.40)
  kj   <- c(food=0.06, paper=0.04)
  phi  <- 1
  f    <- 0
  gwp  <- 28
  ox   <- 0.1
  frac <- 0.5
  mcf  <- 1
  val <- calcSWDSEmissionsYearly(
    W = waste_data,
    DOCj=docj,
    k_j=kj,
    phi_y=phi,
    f_y=f,
    GWP_CH4=gwp,
    OX=ox,
    F=0.5,
    DOCf_y=frac,
    MCF_y=mcf,
    year_target=2
  )
  expect_true(val > 0)
  expect_type(val, "double")
})

test_that("calcSWDSEmissionsSimplified returns numeric", {
  waste_data <- data.frame(
    year = 1:5,
    mass_tonnes = c(100,120,140,160,180)
  )
  val <- calcSWDSEmissionsSimplified(
    W=waste_data,
    phi_y=1, f_y=0, GWP_CH4=28,
    climate_zone="tropical_wet",
    year_target=5
  )
  expect_true(is.numeric(val))
  expect_true(val > 0)
})
'
writeLines(test_calcSWDSEmissions_R, "SWDSFOD/tests/testthat/test_calcSWDSEmissions.R")
