context("Internal prediction and optimisation methods")
	
test_that("getNetworkFromDataframe returns population list", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), population=c(10,20,30,40), x=c(-5,-4,-3,-2), y=c(-1,0,1,2))
	expectedpopulation <- c(10,20,30,40)
	expect_equal(getNetworkFromDataframe(testdataframe)$population, expectedpopulation)
})

test_that("getNetworkFromDataframe returns distance matrix", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), population=c(10,20,30,40), x=c(-5,-4,-3,-2), y=c(-1,0,1,2))
	d <- function(x, y) {
		p1 = testdataframe$x[x]
		p2 = testdataframe$y[x]
		q1 = testdataframe$x[y]
		q2 = testdataframe$y[y]
		return(sqrt((p1-q1)^2 + (p2-q2)^2))
	}
	expecteddistance <- matrix(c(d(1,1),d(1,2),d(1,3),d(1,4),d(2,1),d(2,2),d(2,3),d(2,4),d(3,1),d(3,2),d(3,3),d(3,4),d(4,1),d(4,2),d(4,3),d(4,4)), nrow=4, dimnames=list(c(1,2,3,4),c(1,2,3,4)))
	
	actual <- getNetworkFromDataframe(testdataframe)$distance_matrix
	expect_equal(actual, expecteddistance)
})

test_that("getNetworkFromDataframe returns coordinates", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), population=c(10,20,30,40), x=c(-5,-4,-3,-2), y=c(-1,0,1,2))
	
	expectedcoords <- matrix(c(-5,-4,-3,-2,-1,0,1,2), nrow=4, dimnames=list(c(),c("x","y")))
	
	actual <- getNetworkFromDataframe(testdataframe)$coordinates
	expect_equal(actual, expectedcoords)
})

test_that("getNetworkFromDataframe returns locations", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), population=c(10,20,30,40), x=c(-5,-4,-3,-2), y=c(-1,0,1,2))
	
	expectedlocations <- c(1,2,3,4)
	
	actual <- getNetworkFromDataframe(testdataframe)$locations
	expect_equal(actual, expectedlocations)
})

test_that("makePredictionModel with default parameters creates correct object", {
	actual <- makePredictionModel("test")
  default_flux_model_params  <- c(theta=0.9)
	expect_true(is(actual, "prediction_model"))
	expect_equal(actual$dataset, "test")
	expect_equal(actual$min_network_pop, 50000)
  expect_is(actual$flux_model, "flux")
	expect_true(actual$symmetric)
	expect_equal(actual$flux_model$params, default_flux_model_params)
})

test_that("makePredictionModel with non-default parameters creates correct object", {
	actual <- makePredictionModel("test", 1, gravity(), FALSE)
	expected_flux_model_params  <- c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01)
	expected_flux_model_flux  <- gravityFlux
	expect_is(actual, "prediction_model")
	expect_equal(actual$dataset, "test")
	expect_equal(actual$min_network_pop, 1)
	expect_is(actual$flux_model, "flux")
  expect_equal(actual$flux_model$flux, expected_flux_model_flux)
	expect_equal(actual$flux_model$params, expected_flux_model_params)
	expect_false(actual$symmetric)
})

test_that("analysePredictionUsingdPois using simplest possible matrices returns 4 on identical matrices", {
	observed <- matrix(c(0,1,1,0), nrow=2)
	predicted = list()
	predicted$prediction <- matrix(c(0,1,1,0), nrow=2)
	
	actual <- analysePredictionUsingdPois(predicted, observed)
	expect_equal(actual, 4)
})

test_that("analysePredictionUsingdPois using simplest possible matrices returns correct value on different matrices", {
	observed <- matrix(c(0,1,1,0), nrow=2)
	predicted = list()
	predicted$prediction <- matrix(c(0,2,2,0), nrow=2)
	
	actual <- analysePredictionUsingdPois(predicted, observed)
	expect_equal(actual, 5.227411277760218411004)
})

test_that("predict.prediction_model uses the correct version of getNetwork", {
	predictionModel = list(flux_model = gravity(), symmetric = FALSE)
	with_mock(
	  `movement:::getNetwork` = function(x, min) list(distance_matrix = NULL, population = NULL, name = "getNetwork"),
    `movement:::getNetworkFromDataframe` = function(x, min) list(distance_matrix = NULL, population = NULL, name = "getNetworkFromDataframe"),
		`movement:::movement.predict` = function(distance, population, flux, symmetric, theta, ...) NULL,
		prediction <- predict.prediction_model(predictionModel),
		expect_equal(predict.prediction_model(predictionModel)$net$name, "getNetwork"),
		expect_equal(predict.prediction_model(predictionModel, data.frame(c(1,1)))$net$name, "getNetworkFromDataframe")
	)
})

test_that("predict.prediction_model calls movement.predict with the correct flux method", {
	gravityPredictionModel = list(flux_model = gravity(), symmetric = FALSE)
	radiationPredictionModel = list(flux_model = radiationWithSelection(), symmetric = FALSE)
	with_mock(`movement:::getNetwork` = function(x, min) list(distance_matrix = NULL, population = NULL),
    `movement:::getNetworkFromDataframe` = function(x, min) list(distance_matrix = NULL, population = NULL),
		`movement:::gravityFlux` = function() return ("gravity"),
		`movement:::radiationWithSelectionFlux` = function() return("radiation with selection"),
		`movement:::movement.predict` = function(distance, population, flux, symmetric, theta, ...) return (flux()),
		expect_equal(predict.prediction_model(gravityPredictionModel)$prediction, "gravity"),
		expect_equal(predict.prediction_model(radiationPredictionModel)$prediction, "radiation with selection")
	)
})

test_that("predict.prediction_model correctly returns movement matrix with is consistent with the location data provided", {

  predictionModel <- list(dataset=NULL, flux_model = gravity(), min_network_pop=50000, symmetric = FALSE)
  testMatrix  <- as.movement_matrix(matrix(c(0,1,2,0,4,5,5,6,7),nrow=3))
  testLocationData  <- as.location_dataframe(data.frame(location=c(4335,4426, 4427), population=c(10,20,19), x=c(-1,-1,-2), y=c(-5,-5,-6)))
  with_mock(`movement:::movement.predict` = function(distance, population, flux, symmetric, theta, ...) return (testMatrix),
            prediction  <- predict.prediction_model(predictionModel, testLocationData),
            expect_true(consistencyCheckMovementMatrixLocationDataframe(prediction$prediction, testLocationData))          
  )
})

test_that("fittingWrapper calls predictedresults with correct parameters", {
	with_mock(`movement:::predict.prediction_model` = function(x, y, ...) { return (paste(x,y,..., sep=",", collapse=","))},
	          `movement:::analysePredictionUsingdPois` = function(x, y) return (x),
            `movement:::transformFluxObjectParameters` = function(x, y, ...) return (x),
            expect_equal(fittingWrapper(c(1,1), list(params = c(1,1)), c(1,2), c(3,4)), "c(1, 1),3,FALSE,FALSE,,list(params = c(1, 1)),4,FALSE,FALSE,")
	)
})

test_that("fittingWrapper calls analysePredictionUsingdPois with correct parameters", {
	with_mock(`movement:::predict.prediction_model` = function(x, y, ...) return ("predictedResults"),
	          `movement:::analysePredictionUsingdPois` = function(x, y) return (paste(x,y,sep=",",collapse=",")),
	          `movement:::transformFluxObjectParameters` = function(x, y, ...) return (x),
            expect_equal(fittingWrapper(c(1,1), list(params = c(1,1)), c(1,2), c(3,4)), "predictedResults,1,predictedResults,2")
	)
})