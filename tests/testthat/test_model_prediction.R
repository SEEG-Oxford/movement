library(movement)
context("Model Prediction")

test_that("gravity.flux gives expected results with default parameters", {
	i <- 1
	j <- 2
	distance <- matrix(c(0,1,1,0),nrow=2)
	population <- c(1000,2000)
	expect_equal(gravity.flux(i, j, distance, population), c(617.0338627200094379077,759.6577929323736952938))
})