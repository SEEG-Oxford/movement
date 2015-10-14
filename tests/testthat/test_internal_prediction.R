library(movement)
context("Internal prediction and optimisation methods")

	
test_that("get.network.fromdataframe returns population list", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	expectedpopulation <- c(10,20,30,40)
	expect_equal(get.network.fromdataframe(testdataframe)$population, expectedpopulation)
})

test_that("get.network.fromdataframe returns distance matrix", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	d <- function(x, y) {
		p1 = testdataframe$long_origin[x]
		p2 = testdataframe$lat_origin[x]
		q1 = testdataframe$long_origin[y]
		q2 = testdataframe$lat_origin[y]
		return(sqrt((p1-q1)^2 + (p2-q2)^2))
	}
	expecteddistance <- matrix(c(d(1,1),d(1,2),d(1,3),d(1,4),d(2,1),d(2,2),d(2,3),d(2,4),d(3,1),d(3,2),d(3,3),d(3,4),d(4,1),d(4,2),d(4,3),d(4,4)), nrow=4, dimnames=list(c(1,2,3,4),c(1,2,3,4)))
	
	actual <- get.network.fromdataframe(testdataframe)$distance_matrix
	expect_equal(actual, expecteddistance)
})

test_that("get.network.fromdataframe returns coordinates", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	
	expectedcoords <- matrix(c(-5,-4,-3,-2,-1,0,1,2), nrow=4, dimnames=list(c(),c("x","y")))
	
	actual <- get.network.fromdataframe(testdataframe)$coordinates
	expect_equal(actual, expectedcoords)
})

test_that("get.network.fromdataframe returns locations", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	
	expectedlocations <- c(1,2,3,4)
	
	actual <- get.network.fromdataframe(testdataframe)$locations
	expect_equal(actual, expectedlocations)
})

test_that("movementmodel with default parameters creates correct object", {
	actual <- movementmodel("test")
	expect_true(is(actual, "movementmodel"))
	expect_equal(actual$dataset, "test")
	expect_equal(actual$min_network_pop, 50000)
	expect_equal(actual$predictionmodel, "original radiation")
	expect_true(actual$symmetric)
	expect_equal(actual$modelparams, 0.1)
})

test_that("movementmodel with non-default parameters creates correct object", {
	actual <- movementmodel("test", 1, "radiation with selection", FALSE, 0.2)
	expect_true(is(actual, "movementmodel"))
	expect_equal(actual$dataset, "test")
	expect_equal(actual$min_network_pop, 1)
	expect_equal(actual$predictionmodel, "radiation with selection")
	expect_false(actual$symmetric)
	expect_equal(actual$modelparams, 0.2)
})

test_that("analysepredictionusingdpois using simplest possible matrices returns 4 on identical matrices", {
	observed <- matrix(c(0,1,1,0), nrow=2)
	predicted = list()
	predicted$prediction <- matrix(c(0,1,1,0), nrow=2)
	
	actual <- analysepredictionusingdpois(predicted, observed)
	expect_equal(actual, 4)
})

test_that("analysepredictionusingdpois using simplest possible matrices returns correct value on different matrices", {
	observed <- matrix(c(0,1,1,0), nrow=2)
	predicted = list()
	predicted$prediction <- matrix(c(0,2,2,0), nrow=2)
	
	actual <- analysepredictionusingdpois(predicted, observed)
	expect_equal(actual, 5.227411277760218411004)
})

test_that("predict.movementmodel uses the correct version of get.network", {
	predictionModel = list(predictionmodel = "gravity", symmetric = FALSE, modelparams = NULL)
	with_mock(get.network = function(x, min) list(distance_matrix = NULL, population = NULL, name = "get.network"),
		get.network.fromdataframe = function(x, min) list(distance_matrix = NULL, population = NULL, name = "get.network.fromdataframe"),
		movement.predict = function(distance, population, flux, symmetric, theta, ...) NULL,
		expect_equal(predict.movementmodel(predictionModel)$net$name, "get.network"),
		expect_equal(predict.movementmodel(predictionModel, data.frame(c(1,1)))$net$name, "get.network.fromdataframe")
	)
})

test_that("predict.movementmodel calls movement.predict with the correct flux method", {
	gravityPredictionModel = list(predictionmodel = "gravity", symmetric = FALSE, modelparams = NULL)
	radiationPredictionModel = list(predictionmodel = "radiation with selection", symmetric = FALSE, modelparams = NULL)
	with_mock(get.network = function(x, min) list(distance_matrix = NULL, population = NULL),
		get.network.fromdataframe = function(x, min) list(distance_matrix = NULL, population = NULL),
		gravity.flux = function() return ("gravity"),
		continuum.flux = function() return("continuum"),
		movement.predict = function(distance, population, flux, symmetric, theta, ...) return (flux()),
		expect_equal(predict.movementmodel(gravityPredictionModel)$prediction, "gravity"),
		expect_equal(predict.movementmodel(radiationPredictionModel)$prediction, "continuum")
	)
})

test_that("fittingwrapper calls predictedresults with correct parameters", {
	with_mock(predict.movementmodel = function(x, y, ...) return (paste(x,y,..., sep=",", collapse=",")),
		analysepredictionusingdpois = function(x, y) return (x),
		expect_equal(fittingwrapper(c(1,1), list(), c(1,2), c(3,4)), "c(1, 1),3,c(1, 1),4")
	)
})

test_that("fittingwrapper calls analysepredictionusingdpois with correct parameters", {
	with_mock(predict.movementmodel = function(x, y, ...) return ("predictedResults"),
		analysepredictionusingdpois = function(x, y) return (paste(x,y,sep=",",collapse=",")),
		expect_equal(fittingwrapper(c(1,1), list(), c(1,2), c(3,4)), "predictedResults,1,predictedResults,2")
	)
})