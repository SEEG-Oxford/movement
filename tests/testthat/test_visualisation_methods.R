library(movement)
context("Visualisation methods")

test_that("showprediction.prediction_model passes correct parameters to show.prediction", {
	predictionModel = list(net="network",prediction="move",dataset="raster")
	with_mock(show.prediction = function(x,y,z,...) return(paste(x,y,z,...,sep=',',collaspe=',')),
		expect_equal(showprediction.prediction_model(predictionModel), "network,raster,move,,"))
})