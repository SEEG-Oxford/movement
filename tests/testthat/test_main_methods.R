library(movement)
library(raster)
context("Main Interface Methods")

test_that("predict.optimisedmodel returns list of correct data when given a RasterLayer", {
	predictionModel <- list(trainingresults=NULL)
	dataframe <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
	with_mock(`movement:::predict.movementmodel` = function(x) {
              return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))
            },
            expect_equal(predict.optimisedmodel(predictionModel,dataframe),list(df_locations=data.frame(location=1,pop=1,coordinates=1),movement_matrix=2))
	)
})

test_that("predict.optimisedmodel returns list of correct data when given a data.frame", {
  predictionModel <- list(trainingresults=NULL)
  class(predictionModel) <- 'optimisedmodel'
  dataframe <- data.frame(c(1))    
  with_mock(# note: need to specify explicit the environment of the function which will be replaced by a mock implementation
            `movement:::predict.movementmodel` = function(x,...) {
              return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))
            },
            expected_list <- list(df_locations=data.frame(location=1,pop=1,coordinates=1), movement_matrix = 2),
            actual_predict_optimisedmodel  <- predict.optimisedmodel(predictionModel,dataframe), 
            expect_equal(actual_predict_optimisedmodel, expected_list)
  )  
})
  
test_that("predict.optimisedmodel throws an error if given the wrong type", {
	predictionModel <- list(trainingresults=NULL)
	class(predictionModel) <- 'optimisedmodel'
	dataframe <- 1
	expect_error(predict(predictionModel,dataframe),"Error: Expected parameter `newdata` to be either a RasterLayer or a data.frame")
})

locations <- c("a","b","c")
coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
population <- c(1000,2000,3000)
data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
class(data) <- c('location_dataframe', 'data.frame')
movementData <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
class(movementData) <- c('movement_matrix', 'matrix') 
assign("movementData", movementData, envir = .GlobalEnv)
assign("data", data, envir = .GlobalEnv)

test_that("movement function throws an error if given the wrong flux type", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
  # next file is still failing 
  expect_error(movement(movementData ~ data, "dummy flux object"), "Error: Unknown flux model type given. The input 'flux_model' has to be a flux object.")
})

notMovementMatrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)

test_that("movement function throws an error if given the wrong matrix type", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_false(is.movement_matrix(notMovementMatrix))  # ensure that matrix is not of expected class  
  expect_error(movement(notMovementMatrix ~ data, radiationWithSelection()))
})

test_that("movement sets correct parameters and bounds for original radiation model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_data, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_data, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},   
		`movement:::predict.movementmodel` = function(predictionModel, location_data, progress) {      
			return (list(prediction=NULL))
		},		
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
    actual_movement_object <- movement(movementData ~ data, originalRadiation()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for uniform selection model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, uniformSelection()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for radiation with selection model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, radiationWithSelection()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.1,lambda=0.2))
	)
})

test_that("movement sets correct parameters and bounds for intervening opportunities model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, interveningOpportunities()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.001,L=0.00001))
	)
})

test_that("movement sets correct parameters and bounds for gravity model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, gravity()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01))
	)
})

test_that("movement sets correct parameters and bounds for gravity with distance model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
  with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
  },
  `movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
    return (list(modelparams=NULL,prediction=NULL))
  },
  `movement:::analysepredictionusingdpois` = function(x, y) return (1),
  actual_movement_object <- movement(movementData ~ data, gravityWithDistance()),
  expect_equal(actual_movement_object$optimisationresults$par, c(theta1=0.01, alpha1=0.06, beta1=0.03, gamma1=0.01, delta=0.5, theta2=0.01, alpha2=0.06, beta2=0.03, gamma2=0.01))
  )
})

test_that("movement creates population_data correctly", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptoptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.movementmodel` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysepredictionusingdpois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, gravity()),
		expected_population_data  <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]),
    class(expected_population_data)  <- c('location_dataframe', 'data.frame'),
    expect_equal(actual_movement_object$optimisationresults$inputs$population_data, expected_population_data)
	)
})

test_that("predict.flux throws an error if given the wrong location_dataframe parameter", {
  flux <- originalRadiation()
  dataframe <- 1
  expect_error(predict(flux,dataframe),"Error: Expected parameter `location_dataframe` to be either a RasterLayer or a data.frame")
})

test_that("predict.flux returns list of correct data when given a RasterLayer", {
  flux <- originalRadiation()
  raster <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
  with_mock(`movement:::predict.movementmodel` = function(x) {
    return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))
  },
  expect_equal(predict(flux,raster),list(df_locations=data.frame(location=1,population=1,coordinates=1),movement_matrix=2))
  )
})

test_that("predict.flux returns list of correct data when given a data.frame", {
  flux <- originalRadiation()
  dataframe <- data.frame(c(1))    
  with_mock(
    `movement:::predict.movementmodel` = function(x,...) {
      return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))
    },
    expected_list <- list(df_locations=data.frame(location=1,population=1,coordinates=1), movement_matrix = 2),
    actualPredictMovements  <- predict(flux,dataframe), 
    expect_equal(actualPredictMovements, expected_list)
  )  
})

