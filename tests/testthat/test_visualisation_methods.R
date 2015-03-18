library(movement)
context("Visualisation methods")

test_that("showprediction.movementmodel passes correct parameters to show.prediction", {
	predictionModel = list(net="network",prediction="move",dataset="raster")
	with_mock(show.prediction = function(x,y,z,...) return(paste(x,y,z,...,sep=',',collaspe=',')),
		expect_equal(showprediction.movementmodel(predictionModel), "network,raster,move,,"))
})