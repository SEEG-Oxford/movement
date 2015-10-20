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