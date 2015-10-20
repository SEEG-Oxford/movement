library(movement)
context("Model definitions")

test_that("original radiation model is of class flux",{
  model  <- original.radiation()
  expect_is(model, "flux")
})

test_that("original radiation model has expected default values",{
  default_params  <- c(theta=0.9)
  expected_flux  <- continuum.flux
  model  <- original.radiation()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("original radiation model can be called with new set of params",{
  expected_params  <- c(theta=1)
  model  <- original.radiation(expected_params)
  expect_equal(model$params, expected_params)
})

test_that("radiation with selection model is of class flux",{
  model  <- radiation.with.selection()
  expect_is(model, "flux")
})

test_that("radiation with selection model has expected default values",{
  default_params  <- c(theta=0.1,lambda=0.2)
  expected_flux  <- continuum.flux
  model  <- radiation.with.selection()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("radiation with selection model can be called with new set of params",{
  expected_params  <- c(theta=1,lambda=2)
  model  <- radiation.with.selection(expected_params)
  expect_equal(model$params, expected_params)
})

test_that("uniform selection model is of class flux",{
  model  <- uniform.selection()
  expect_is(model, "flux")
})

test_that("uniform selection model has expected default values",{
  default_params  <- c(theta=0.9)
  expected_flux  <- continuum.flux
  model  <- uniform.selection()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("uniform selection model can be called with new set of params",{
  expected_params  <- c(theta=1)
  model  <- uniform.selection(expected_params)
  expect_equal(model$params, expected_params)
})

test_that("intervening opportunities model is of class flux",{
  model  <- intervening.opportunities()
  expect_is(model, "flux")
})

test_that("intervening opportunities model has expected default values",{
  default_params  <- c(theta=0.001, L=0.00001)
  expected_flux  <- continuum.flux
  model  <- intervening.opportunities()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("intervening opportunities model can be called with new set of params",{
  expected_params  <- c(theta=1, L=2)
  model  <- intervening.opportunities(expected_params)
  expect_equal(model$params, expected_params)
})

test_that("gravity model is of class flux",{
  model  <- gravity()
  expect_is(model, "flux")
})

test_that("gravity model has expected default values",{
  default_params  <- c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01)
  expected_flux  <- gravity.flux
  model  <- gravity()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("gravity model can be called with new set of params",{
  expected_params  <- c(theta=1, alpha=2, beta=5, gamma=0.5)
  model  <- gravity(expected_params)
  expect_equal(model$params, expected_params)
})

test_that("gravity with distance is of class flux",{
  model  <- gravity.with.distance()
  expect_is(model, "flux")
})

test_that("gravity with distance model has expected default values",{
  default_params  <- c(theta1=0.01, alpha1=0.06, beta1=0.03, gamma1=0.01, delta=0.5, theta2=0.01, alpha2=0.06, beta2=0.03, gamma2=0.01)
  expected_flux  <- gravitywithdistance.flux
  model  <- gravity.with.distance()
  expect_equal(model$params, default_params)
  expect_equal(model$flux, expected_flux)
})

test_that("gravity with distance model can be called with new set of params",{
  expected_params  <- c(theta1=1, alpha1=2, beta1=3, gamma1=4, delta=1, theta2=0.5, alpha2=1.5, beta2=2.5, gamma2=3.5)
  model  <- gravity(expected_params)
  expect_equal(model$params, expected_params)
})
