library(movement)
library(raster)
context("Main Interface Methods")

test_that("predict.movement_model returns list of correct data when given a RasterLayer", {
	predictionModel <- list(training_results=NULL)
	dataframe <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
	with_mock(`movement:::predict.prediction_model` = function(x, ...) {
              return (list(net=list(locations=1,population=1,coordinates=1),prediction=matrix(2)))
            },
	          expected_predict_movement_model  <- list(net=list(locations=1,population=1,coordinates=1),movement_matrix = as.movement_matrix(matrix(2)), dataset = dataframe),
            class(expected_predict_movement_model)  <- "movement_predictions",
	          actual_predict_movement_model  <- predict.movement_model(predictionModel,dataframe),
            expect_equal(actual_predict_movement_model, expected_predict_movement_model)
	)
})


test_that("predict.movement_model returns list of correct data when given a data.frame", {
  predictionModel <- list(training_results=NULL)
  class(predictionModel) <- 'movement_model'
  dataframe <- data.frame(c(1))    
  with_mock(# note: need to specify explicit the environment of the function which will be replaced by a mock implementation
            `movement:::predict.prediction_model` = function(x,...) {
              return (list(net=list(locations=1,population=1,coordinates=1),prediction=matrix(2)))
            },
            expected_predict_movement_model <- list(net=list(locations=1,population=1,coordinates=1), movement_matrix = as.movement_matrix(matrix(2)), dataset = dataframe),
            class(expected_predict_movement_model)  <- "movement_predictions",
            actual_predict_movement_model  <- predict.movement_model(predictionModel,dataframe), 
            expect_equal(actual_predict_movement_model, expected_predict_movement_model)
  )  
})
  
test_that("predict.movement_model throws an error if given the wrong type", {
	predictionModel <- list(training_results=NULL)
	class(predictionModel) <- 'movement_model'
	dataframe <- 1
	expect_error(predict(predictionModel,dataframe),"Error: Expected parameter `new_data` to be either a RasterLayer or a data.frame")
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

test_that("movement function throws an error if not given formula as expected", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # ensure that matrix is not of expected class  
  expect_error(movement(movementData, data, radiationWithSelection()), "Error in extractArgumentsFromFormula")
})

movement_matrix_with_non_integer_values  <- matrix(c(0.192,1.2,2.02,3.34,0.42,4.921,5.282,6.282,0.012),nrow=3) # movement cannot be given in decimal numbers (must be integer!)
class(movement_matrix_with_non_integer_values) <- c('movement_matrix', 'matrix') 
assign("movement_matrix_with_non_integer_values", movement_matrix_with_non_integer_values, envir = .GlobalEnv)

test_that("movement function throws an error if given an invalid movement matrix", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movement_matrix_with_non_integer_values))  # check that the data are of correct class  
  # next file is still failing 
  expect_warning(movement(movement_matrix_with_non_integer_values ~ data, radiationWithSelection()), 
                 "The given observed movement matrix contains non-integer values. Rounding was used to receive a valid movement matrix.")
})

test_that("movement function correctly round the movement_matrix when containing non-integer values", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movement_matrix_with_non_integer_values))  # check that the data are of correct class  
  with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_data, movement_matrix_with_non_integer_values, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_data, movement_matrix=movement_matrix_with_non_integer_values, progress=progress, hessian=hessian)))
  },   
  `movement:::predict.prediction_model` = function(predictionModel, location_data, progress) {      
    return (list(prediction=NULL))
  },		
  `movement:::analysePredictionUsingdPois` = function(x, y) return (1),
  actual_movement_object <- movement(movement_matrix_with_non_integer_values ~ data, originalRadiation()),
  expected_rounded_matrix  <- matrix(c(0,1,2,3,0,5,5,6,0),nrow=3),
  class(expected_rounded_matrix)  <- c('movement_matrix', 'matrix'),
  actual_movement_matrix  <- actual_movement_object$training_results$dataset$movement_matrix,
  expect_equal(actual_movement_matrix, expected_rounded_matrix)
  )
})

test_that("movement function print warning when inconsistency in locations between movement matrix and location data are found", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
  with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_data, movementData, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_data, movement_matrix=movementData, progress=progress, hessian=hessian)))
  },   
  `movement:::predict.prediction_model` = function(predictionModel, location_data, progress) {      
    return (list(prediction=NULL))
  },		
  `movement:::analysePredictionUsingdPois` = function(x, y) return (1),  
  expect_warning(movement(movementData ~ data, originalRadiation()), 
                 "The given movement_matrix and the location_dataframe having non-matching location information.")
  )
})

test_that("movement sets correct parameters and bounds for original radiation model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_data, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_data, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},   
		`movement:::predict.prediction_model` = function(predictionModel, location_data, progress) {      
			return (list(prediction=NULL))
		},		
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
    actual_movement_object <- movement(movementData ~ data, originalRadiation()),
		expect_equal(actual_movement_object$optimisation_results$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for uniform selection model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, uniformSelection()),
		expect_equal(actual_movement_object$optimisation_results$par, c(theta=0.9))
	)
})

test_that("movement sets correct parameters and bounds for radiation with selection model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, radiationWithSelection()),
		expect_equal(actual_movement_object$optimisation_results$par, c(theta=0.1,lambda=0.2))
	)
})

test_that("movement sets correct parameters and bounds for intervening opportunities model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, interveningOpportunities()),
		expect_equal(actual_movement_object$optimisation_results$par, c(theta=0.001,L=0.00001))
	)
})

test_that("movement sets correct parameters and bounds for gravity model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, gravity()),
		expect_equal(actual_movement_object$optimisation_results$par, c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01))
	)
})

test_that("movement sets correct parameters and bounds for gravity with distance model", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
  with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
  },
  `movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
    return (list(modelparams=NULL,prediction=NULL))
  },
  `movement:::analysePredictionUsingdPois` = function(x, y) return (1),
  actual_movement_object <- movement(movementData ~ data, gravityWithDistance()),
  expect_equal(actual_movement_object$optimisation_results$par, c(theta1=0.01, alpha1=0.06, beta1=0.03, gamma1=0.01, delta=0.5, theta2=0.01, alpha2=0.06, beta2=0.03, gamma2=0.01))
  )
})

test_that("movement creates population_data correctly", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
	with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
			return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
		},
		`movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
			return (list(modelparams=NULL,prediction=NULL))
		},
		`movement:::analysePredictionUsingdPois` = function(x, y) return (1),
		actual_movement_object <- movement(movementData ~ data, gravity()),
		expected_population_data  <- data.frame(location = locations, population = population, x = coords[,1], y = coords[,2]),
    class(expected_population_data)  <- c('location_dataframe', 'data.frame'),
    expect_equal(actual_movement_object$optimisation_results$inputs$population_data, expected_population_data)
	)
})

test_that("movement creates training_results correctly", {
  expect_true(is.location_dataframe(data)) # check that the data are of correct class
  expect_true(is.movement_matrix(movementData))  # check that the data are of correct class  
  with_mock(`movement:::attemptOptimisation` = function(predictionModel, location_dataframe_origin, movementData, progress, hessian, ...) {
    return (list(par=predictionModel$flux_model$params, value=2,inputs=list(predictionModel=predictionModel, population_data=location_dataframe_origin, movement_matrix=movementData, progress=progress, hessian=hessian)))
  },
  `movement:::predict.prediction_model` = function(predictionModel, location_dataframe_origin, progress) {
    return (list(modelparams=NULL,prediction=NULL))
  },
  `movement:::analysePredictionUsingdPois` = function(x, y) return (1),
  actual_movement_object <- movement(movementData ~ data, gravity()),
  expect_equal(actual_movement_object$training_results$dataset$movement_matrix, movementData),
  expect_equal(actual_movement_object$training_results$dataset$location_dataframe, data),
  expect_equal(actual_movement_object$training_results$flux_model$params, actual_movement_object$optimisation_results$par)
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
  with_mock(`movement:::predict.prediction_model` = function(x, ...) {
    prediction_matrix  <- matrix(c(0,1,2,0),nrow=2)
    return (list(net=list(locations=1,population=1,coordinates=1),prediction=prediction_matrix))
  },
  expected_movement_matrix  <- matrix(c(0,1,2,0),nrow=2),
  class(expected_movement_matrix)  <- c('matrix', 'movement_matrix'),
  expected_list <- list(df_locations=data.frame(location=1,population=1,coordinates=1),movement_matrix=expected_movement_matrix),
  expect_equal(predict(flux,raster), expected_list)
  )
})

test_that("predict.flux returns list of correct data when given a data.frame", {
  flux <- originalRadiation()
  dataframe <- data.frame(c(1))    
  with_mock(
    `movement:::predict.prediction_model` = function(x,...) {
      prediction_matrix  <- matrix(c(0,1,2,0),nrow=2)
      return (list(net=list(locations=1,population=1,coordinates=1),prediction=prediction_matrix))
    },
    expected_movement_matrix  <- matrix(c(0,1,2,0),nrow=2),
    class(expected_movement_matrix)  <- c('matrix', 'movement_matrix'),
    expected_list <- list(df_locations=data.frame(location=1,population=1,coordinates=1), movement_matrix = expected_movement_matrix),
    actualPredictMovements  <- predict(flux,dataframe), 
    expect_equal(actualPredictMovements, expected_list)
  )  
})

test_that("AIC.movement_model returns correct value", {
  dummy_movement_model <- list(call = "call",
                               optimisation_results = list(value = 2),
                               training_results = "training_results",
                               coefficients = "coefficients",
                               df_null = "df_null",
                               df_residual = "df_residual", 
                               null_deviance = "null_deviance",
                               deviance = "deviance",
                               aic = "aic") 
  class(dummy_movement_model)  <- 'movement_model'  
  
  expected_aic  <- 4
  actual_aic  <- AIC(dummy_movement_model)
  expect_equal(actual_aic, expected_aic)
})


