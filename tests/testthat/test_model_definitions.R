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
  expected_theta  <- 1
  expected_params  <- c(theta = expected_theta)
  model  <- original.radiation(theta = expected_theta)
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

test_that("radiation with selection model can be called with new theta value only",{
  expected_theta <- 1
  default_lambda <- 0.2
  expected_params  <- c(theta=expected_theta,lambda=default_lambda)
  model  <- radiation.with.selection(theta = expected_theta)
  expect_equal(model$params, expected_params)
})

test_that("radiation with selection model can be called with new set of params",{
  expected_theta <- 1
  expected_lambda <- 2
  expected_params  <- c(theta=expected_theta,lambda=expected_lambda)
  model  <- radiation.with.selection(theta = expected_theta, lambda = expected_lambda)
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
  expected_theta = 1
  expected_params  <- c(theta=expected_theta)
  model  <- uniform.selection(theta = expected_theta)
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

test_that("intervening opportunities model can be called with new L param value only",{
  default_theta = 0.001
  expected_L = 2
  expected_params  <- c(theta=default_theta, L=expected_L)
  model  <- intervening.opportunities(L = expected_L)
  expect_equal(model$params, expected_params)
})

test_that("intervening opportunities model can be called with new set of params",{
  expected_theta = 1
  expected_L = 2
  expected_params  <- c(theta=expected_theta, L=expected_L)
  model  <- intervening.opportunities(theta = expected_theta, L = expected_L)
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

test_that("gravity model can be called with new set of full params",{
  expected_theta = 1
  expected_alpha = 2
  expected_beta = 5
  expected_gamma = 0.5
  expected_params  <- c(theta=expected_theta, alpha=expected_alpha, beta=expected_beta, gamma=expected_gamma)
  model  <- gravity(theta=expected_theta, alpha=expected_alpha, beta=expected_beta, gamma=expected_gamma)
  expect_equal(model$params, expected_params)
})

test_that("gravity model can be called with new set of partial params",{
  expected_theta = 1
  expected_alpha = 2
  default_beta = 0.03
  expected_gamma = 0.5
  expected_params  <- c(theta=expected_theta, alpha=expected_alpha, beta=default_beta, gamma=expected_gamma)
  model  <- gravity(theta=expected_theta, alpha=expected_alpha, gamma=expected_gamma)
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
  expected_theta1 = 1
  expected_alpha1 = 2
  expected_beta1 = 3
  expected_gamma1 = 4
  expected_delta = 1
  expected_theta2 = 0.1
  expected_alpha2 = 0.2
  expected_beta2 = 0.3
  expected_gamma2 = 0.4  
  expected_params  <- c(theta1=expected_theta1, alpha1=expected_alpha1, beta1=expected_beta1, gamma1=expected_gamma1, delta=expected_delta, theta2=expected_theta2, alpha2=expected_alpha2, beta2=expected_beta2, gamma2=expected_gamma2)
  model  <- gravity.with.distance(theta1=expected_theta1, alpha1=expected_alpha1, beta1=expected_beta1, gamma1=expected_gamma1, delta=expected_delta, theta2=expected_theta2, alpha2=expected_alpha2, beta2=expected_beta2, gamma2=expected_gamma2)
  expect_equal(model$params, expected_params)
})
