library(movement)
library(raster)
context("Main Interface Methods")

# test_that("predict.optimisedmodel returns list of correct data when given a RasterLayer", {
# 	predictionModel <- list(trainingresults=NULL)
# 	dataframe <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
# 	with_mock(predict.movementmodel = function(x) {return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))},
# 		expect_equal(predict.optimisedmodel(predictionModel,dataframe),list(df_locations=data.frame(location=1,pop=1,coordinates=1),movement_matrix=2))
# 	)
# })

# test_that("predict.optimisedmodel returns list of correct data when given a data.frame", {
# 	predictionModel <- list(trainingresults=NULL)
# 	dataframe <- data.frame(c(1))
# 	with_mock(predict.movementmodel = function(x, y) {return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))},
# 		expect_equal(predict.optimisedmodel(predictionModel,dataframe),list(df_locations=data.frame(location=1,pop=1,coordinates=1),movement_matrix=2))
# 	)
# })

test_that("predict.optimisedmodel throws an error if given the wrong type", {
	predictionModel <- list(trainingresults=NULL)
	class(predictionModel) <- 'optimisedmodel'
	dataframe <- 1
	expect_error(predict(predictionModel,dataframe),"Error: Expected parameter `newdata` to be either a RasterLayer or a data.frame")
})

# test_that("movement sets correct parameters and bounds for original radiation model", {
	# locations <- c("a","b","c")
	# coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
	# population <- c(1000,2000,3000)
	# movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
	# with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
			# return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
		# },
		# predict.movementmodel = function(predictionModel, population_data, progress) {
			# return (list(modelparams=NULL,prediction=NULL))
		# },
		# analysepredictionusingdpois = function(x, y) return (1),
		# expect_equal((movement(locations, coords, population, movement_matrix, "original radiation"))$optimisationresults$par, c(theta=0.9)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "original radiation"))$optimisationresults$inputs$upper, c(Inf)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "original radiation"))$optimisationresults$inputs$lower, c(0))
	# )
# })

# test_that("movement sets correct parameters and bounds for original radiation model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "original radiation"))$optimisationresults$par, c(theta=0.9))
# 	)
# })

# test_that("movement sets correct parameters and bounds for uniform selection model", {
	# locations <- c("a","b","c")
	# coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
	# population <- c(1000,2000,3000)
	# movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
	# with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
			# return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
		# },
		# predict.movementmodel = function(predictionModel, population_data, progress) {
			# return (list(modelparams=NULL,prediction=NULL))
		# },
		# analysepredictionusingdpois = function(x, y) return (1),
		# expect_equal((movement(locations, coords, population, movement_matrix, "uniform selection"))$optimisationresults$par, c(theta=0.9)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "uniform selection"))$optimisationresults$inputs$upper, c(Inf)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "uniform selection"))$optimisationresults$inputs$lower, c(0))
	# )
# })

# test_that("movement sets correct parameters and bounds for uniform selection model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "uniform selection"))$optimisationresults$par, c(theta=0.9))
# 	)
# })

# test_that("movement sets correct parameters and bounds for radiation with selection model", {
	# locations <- c("a","b","c")
	# coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
	# population <- c(1000,2000,3000)
	# movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
	# with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
			# return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
		# },
		# predict.movementmodel = function(predictionModel, population_data, progress) {
			# return (list(modelparams=NULL,prediction=NULL))
		# },
		# analysepredictionusingdpois = function(x, y) return (1),
		# expect_equal((movement(locations, coords, population, movement_matrix, "radiation with selection"))$optimisationresults$par, c(theta=0.1,lambda=0.2)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "radiation with selection"))$optimisationresults$inputs$upper, c(Inf,1)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "radiation with selection"))$optimisationresults$inputs$lower, c(0,0))
	# )
# })

# test_that("movement sets correct parameters and bounds for radiation with selection model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "radiation with selection"))$optimisationresults$par, c(theta=0.1,lambda=0.2))
# 	)
# })

# test_that("movement sets correct parameters and bounds for intervening opportunities model", {
	# locations <- c("a","b","c")
	# coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
	# population <- c(1000,2000,3000)
	# movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
	# with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
			# return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
		# },
		# predict.movementmodel = function(predictionModel, population_data, progress) {
			# return (list(modelparams=NULL,prediction=NULL))
		# },
		# analysepredictionusingdpois = function(x, y) return (1),
		# expect_equal((movement(locations, coords, population, movement_matrix, "intervening opportunities"))$optimisationresults$par, c(theta=0.001,L=0.00001)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "intervening opportunities"))$optimisationresults$inputs$upper, c(Inf,Inf)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "intervening opportunities"))$optimisationresults$inputs$lower, c(1e-20, 1e-05))
	# )
# })

# test_that("movement sets correct parameters and bounds for intervening opportunities model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "intervening opportunities"))$optimisationresults$par, c(theta=0.001,L=0.00001))
# 	)
# })

# test_that("movement sets correct parameters and bounds for gravity model", {
	# locations <- c("a","b","c")
	# coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
	# population <- c(1000,2000,3000)
	# movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
	# with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
			# return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
		# },
		# predict.movementmodel = function(predictionModel, population_data, progress) {
			# return (list(modelparams=NULL,prediction=NULL))
		# },
		# analysepredictionusingdpois = function(x, y) return (1),
		# expect_equal((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$par, c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$inputs$upper, c(Inf,Inf, Inf, Inf)),
		# expect_equal((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$inputs$lower, c(1e-20, -Inf, -Inf, -Inf))
	# )
# })

# test_that("movement sets correct parameters and bounds for gravity model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$par, c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01))
# 	)
# })

# test_that("movement throws error for unknown model", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, upper, lower, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		expect_error(movement(locations, coords, population, movement_matrix, "no gravity"),"Error: Unknown model type given")
# 	)
# })

#test_that("movement creates population_data correctly", {
#	locations <- c("a","b","c")
#	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
#	population <- c(1000,2000,3000)
#	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
#	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, #hessian, upper, lower, ...) {
#			return (list(par=predictionModel$modelparams, #value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, #movement_matrix=movement_matrix, progress=progress, hessian=hessian, upper=upper, lower=lower)))
#		},
#		predict.movementmodel = function(predictionModel, population_data, progress) {
#			return (list(modelparams=NULL,prediction=NULL))
#		},
#		analysepredictionusingdpois = function(x, y) return (1),
#		print((movement(locations, coords, population, movement_matrix, #"gravity"))$optimisationresults$inputs$population_data),
#		expect_equal((movement(locations, coords, population, movement_matrix, #"gravity"))$optimisationresults$inputs$population_data, data.frame(origin=locations, #pop_origin=population, long_origin=coords[,1], lat_origin=coords[,2]))
#	)
#})

# test_that("movement creates population_data correctly", {
# 	locations <- c("a","b","c")
# 	coords <- data.frame(c(1,2,3,4,5,6), nrow=3)
# 	population <- c(1000,2000,3000)
# 	movement_matrix <- matrix(c(0,1,2,3,0,4,5,6,0),nrow=3)
# 	with_mock(attemptoptimisation = function(predictionModel, population_data, movement_matrix, progress, hessian, ...) {
# 			return (list(par=predictionModel$modelparams, value=2,inputs=list(predictionModel=predictionModel, population_data=population_data, movement_matrix=movement_matrix, progress=progress, hessian=hessian)))
# 		},
# 		predict.movementmodel = function(predictionModel, population_data, progress) {
# 			return (list(modelparams=NULL,prediction=NULL))
# 		},
# 		analysepredictionusingdpois = function(x, y) return (1),
# 		print((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$inputs$population_data),
# 		expect_equal((movement(locations, coords, population, movement_matrix, "gravity"))$optimisationresults$inputs$population_data, data.frame(origin=locations, pop_origin=population, long_origin=coords[,1], lat_origin=coords[,2]))
# 	)
# })