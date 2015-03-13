library(movement)
context("Model Prediction")

test_that("gravity.flux gives expected results with default parameters", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population), c(617.0338627200094379077,759.6577929323736952938))
})

test_that("gravity.flux gives expected results with symmetric = TRUE", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population, symmetric=TRUE), 1376.6916556523831332015)
})

test_that("gravity.flux gives expected results with minpop set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population, symmetric=TRUE, minpop=1001), 0)
})

test_that("gravity.flux gives expected results with maxrange set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population, symmetric=TRUE, maxrange=0.1), Inf)
})

test_that("gravity.flux gives expected results with changed theta set", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population, symmetric=TRUE, theta=c(2, 0.5, 0.2, 2)), 645.3029881934739933058)
})