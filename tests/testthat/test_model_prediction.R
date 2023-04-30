context("Model Prediction")

test_that("gravityWithDistanceFlux gives expected results with default parameters", {
  i <- 1
  j <- 2
  distance <- matrix(c(0,1,1,0),nrow=2)
  population <- c(1000,2000)
  expect_equal(gravityWithDistanceFlux(i, j, distance, population), c(617.0338627200094379077,759.6577929323736952938))
})

test_that("gravityFlux gives expected results with default parameters", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravityFlux(i, j, distance, population), c(617.0338627200094379077,759.6577929323736952938))
})

test_that("gravityFlux gives expected results with symmetric = TRUE", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravityFlux(i, j, distance, population, symmetric=TRUE), 1376.6916556523831332015)
})

test_that("gravityFlux gives expected results with minpop set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravityFlux(i, j, distance, population, symmetric=TRUE, minpop=1001), 0)
})

test_that("gravityFlux gives expected results with maxrange set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravityFlux(i, j, distance, population, symmetric=TRUE, maxrange=0.1), Inf)
})

test_that("gravityFlux gives expected results with changed theta set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravityFlux(i, j, distance, population, symmetric=TRUE, theta=c(2, 0.5, 0.2, 2)), 645.3029881934739933058)
})

test_that("originalRadiationFlux gives expected results", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- originalRadiationFlux(i, j, distance, population)
	expect_equal(actual, c(666.6666666666666287711, 666.6666666666666287711))
})

test_that("radiationWithSelectionFlux gives expected results", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- radiationWithSelectionFlux(i, j, distance, population, theta=c(0.1,0.1))
	expect_equal(actual, c(66.64445184938352895188, 66.64445184938352895188))
})

test_that("interveningOpportunitiesFlux gives expected results with intervening opportunities model", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- interveningOpportunitiesFlux(i, j, distance, population, theta=c(0.1,0.1))
	expect_equal(actual, c(100, 200))
})

test_that("uniformSelectionFlux gives expected results", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- uniformSelectionFlux(i, j, distance, population, theta=0.9)
	expect_equal(actual, c(900, 1800))
})

test_that("uniformSelectionFlux gives expected results in symmetric mode", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- uniformSelectionFlux(i, j, distance, population, theta=0.9, symmetric=TRUE)
	expect_equal(actual, c(2700))
})

test_that("uniformSelectionFlux gives expected results with minpop set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- uniformSelectionFlux(i, j, distance, population, theta=0.9, minpop=1001)
	expect_equal(actual, c(0,0))
})

test_that("uniformSelectionFlux gives expected results with maxrange set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	actual <- uniformSelectionFlux(i, j, distance, population, theta=0.9, maxrange=0.5)
	expect_equal(actual, c(0,0))
})

test_that("movement.predict produces correct result for simple case", {
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	mock_flux <- function(i, j, distance, population, symmetric) return (1)
	actual <- movement.predict(distance, population, flux=mock_flux, progress = FALSE)
	expect_equal(actual, matrix(c(0,NA,1,0), nrow=2))
})

test_that("movement.predict produces correct result for simple case 2 with non-symmetric distances", {
  distance <- matrix(c(0,1,2,1,2,0,2,0,1),nrow=3)
  population <- c(500, 1000,2000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, progress = FALSE)
  expected_matrix  <- matrix(c(0,NA,NA,5,0,NA,5,5,0), nrow = 3)
  expect_equal(actual, expected_matrix)
})

test_that("movement.predict produces correct result for simple case with non-symmetric distances running parallel on 1 cores", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()
  distance <- matrix(c(0,1,2,3, 1,2,3, 0,2,3, 0,1, 3, 0, 1, 2),nrow=4)
  population <- c(500, 1000,2000, 5000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, progress = FALSE, go_parallel = TRUE, number_of_cores = 1)
  expected_matrix  <- matrix(c(0,NA,NA,NA,5,0,NA,NA,5,5,0,NA, 5,5,5,0), nrow = 4)
  expect_equal(actual, expected_matrix)
})

test_that("movement.predict produces correct result for simple case with non-symmetric distances running parallel on 2 cores", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()
  distance <- matrix(c(0,1,2,3, 1,2,3, 0,2,3, 0,1, 3, 0, 1, 2),nrow=4)
  population <- c(500, 1000,2000, 5000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, progress = FALSE, go_parallel = TRUE, number_of_cores = 2)
  expected_matrix  <- matrix(c(0,NA,NA,NA,5,0,NA,NA,5,5,0,NA, 5,5,5,0), nrow = 4)
  expect_equal(actual, expected_matrix)
})

test_that("movement.predict produces correct result for simple case with symmetric distances", {
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	mock_flux <- function(i, j, distance, population, symmetric) return (5)
	actual <- movement.predict(distance, population, flux=mock_flux, symmetric=TRUE, progress = FALSE)
	expect_equal(actual, matrix(c(0,5,5,0), nrow=2))
})

test_that("movement.predict produces correct result for simple case 2 with symmetric distances", {
  distance <- matrix(c(0,1,2,1,2,0,2,0,1),nrow=3)
  population <- c(500, 1000,2000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, symmetric=TRUE, progress = FALSE)
  expected_matrix  <- matrix(c(0,5,5,5,0,5,5,5,0), nrow = 3)
  expect_equal(actual, expected_matrix)
})

test_that("movement.predict produces correct result for simple case with symmetric distances running parallel on 1 cores", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()
  distance <- matrix(c(0,1,2,3, 1,2,3, 0,2,3, 0,1, 3, 0, 1, 2),nrow=4)
  population <- c(500, 1000,2000, 5000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, symmetric=TRUE, progress = FALSE, go_parallel = TRUE, number_of_cores = 1)
  expected_matrix  <- matrix(c(0,5,5,5,5,0,5,5,5,5,0, 5, 5,5,5,0), nrow = 4)
  expect_equal(actual, expected_matrix)
})


test_that("movement.predict produces correct result for simple case with symmetric distances running parallel on 2 cores", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()
  distance <- matrix(c(0,1,2,3, 1,2,3, 0,2,3, 0,1, 3, 0, 1, 2),nrow=4)
  population <- c(500, 1000,2000, 5000)
  mock_flux <- function(i, j, distance, population, symmetric) return (5)
  actual <- movement.predict(distance, population, flux=mock_flux, symmetric=TRUE, progress = FALSE, go_parallel = TRUE, number_of_cores = 2)
  expected_matrix  <- matrix(c(0,5,5,5,5,0,5,5,5,5,0, 5, 5,5,5,0), nrow = 4)
  expect_equal(actual, expected_matrix)
})
