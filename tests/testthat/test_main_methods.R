library(movement)
library(raster)
context("Main Interface Methods")

test_that("predict.optimisedmodel returns list of correct data when given a RasterLayer", {
	predictionModel <- list(trainingresults=NULL)
	dataframe <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
	with_mock(`movement::predict.movementmodel` = function(x) {
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
            `movement::predict.movementmodel` = function(x,...) {
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


test_that("movement function throws an error if given the wrong flux type", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
  expect_error(movement(mm ~ data, "dummy flux object"), "Error: Unknown flux model type given. The input 'flux_model' has to be a flux object.")
})

test_that("movement function throws an error if given the wrong matrix type", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  expect_false(is.movementmatrix(matrix))  # ensure that matrix is not of expected class  
  expect_error(movement(matrix ~ data, radiation.with.selection()))
})

test_that("movement sets correct parameters and bounds for original radiation model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},   
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {      
			return (list(prediction=NULL))
		},		
		analysepredictionusingdpois = function(x, y) return (1),
    actual_movement_object <- movement(mm ~ data, original.radiation()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for uniform selection model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		analysepredictionusingdpois = function(x, y) return (1),
		actual_movement_object <- movement(mm ~ data, uniform.selection()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for radiation with selection model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		analysepredictionusingdpois = function(x, y) return (1),
		actual_movement_object <- movement(mm ~ data, radiation.with.selection()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.1,lambda=0.2))
	)
})

test_that("movement sets correct parameters and bounds for intervening opportunities model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		analysepredictionusingdpois = function(x, y) return (1),
		actual_movement_object <- movement(mm ~ data, intervening.opportunities()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.001,L=0.00001))
	)
})

test_that("movement sets correct parameters and bounds for gravity model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		analysepredictionusingdpois = function(x, y) return (1),
		actual_movement_object <- movement(mm ~ data, gravity()),
		expect_equal(actual_movement_object$optimisationresults$par, c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01))
	)
})

test_that("movement sets correct parameters and bounds for gravity with distance model", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
  with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
  },
  `movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
    return (list(modelparams=NULL,prediction=NULL))
  },
  analysepredictionusingdpois = function(x, y) return (1),
  actual_movement_object <- movement(mm ~ data, gravity.with.distance()),
  expect_equal(actual_movement_object$optimisationresults$par, c(theta1=0.01, alpha1=0.06, beta1=0.03, gamma1=0.01, delta=0.5, theta2=0.01, alpha2=0.06, beta2=0.03, gamma2=0.01))
  )
})

test_that("movement creates population_data correctly", {
  locations <- c("a","b","c")
  coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
  population <- c(1000,2000,3000)
  data <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]) 
  class(data) <- c('locationdataframe', 'data.frame')
  expect_true(is.locationdataframe(data)) # check that the data are of correct class
  mm <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
  class(mm) <- c('movementmatrix', 'matrix') 
  expect_true(is.movementmatrix(mm))  # check that the data are of correct class  
	with_mock(attemptoptimisation = function(predictionModel, locationdataframe_origin, movement_matrix, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=locationdataframe_origin, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
		},
		`movement::predict.movementmodel` = function(predictionModel, locationdataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		analysepredictionusingdpois = function(x, y) return (1),
		actual_movement_object <- movement(mm ~ data, gravity()),
		expect_equal(actual_movement_object$optimisationresults$inputs$population_data, data.frame(origin=locations, pop_origin=population, long_origin=coords[,1], lat_origin=coords[,2]))
	)
})