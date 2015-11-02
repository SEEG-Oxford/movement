library(movement)
context("Visualisation methods")

test_that("plot.prediction_model passes correct parameters to show.prediction", {
	movement_predictions = list(net="network",movement_matrix="move",dataset="raster")
  class(movement_predictions)  <- "movement_predictions"
	with_mock(show.prediction = function(x,y,z,...) return(paste(x,y,z,...,sep=',',collaspe=',')),
            actual_predictions  <- plot(movement_predictions),
            expect_equal(actual_predictions, "network,raster,move,,"))
})