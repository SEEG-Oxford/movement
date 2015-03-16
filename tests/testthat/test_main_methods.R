library(movement)
library(raster)
context("Main Interface Methods")

test_that("predict.optimisedmodel returns list of correct data when given a RasterLayer", {
	predictionModel <- list(trainingresults=NULL)
	dataframe <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
	with_mock(predict.movementmodel = function(x) {return (list(net=list(locations=1,population=1,coordinates=1),prediction=2))},
		expect_equal(predict.optimisedmodel(predictionModel,dataframe),list(df_locations=data.frame(location=1,pop=1,coordinates=1),movement_matrix=2))
	)
})