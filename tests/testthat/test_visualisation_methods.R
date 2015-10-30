library(movement)
context("Visualisation methods")

test_that("showprediction print warning when called with not correct object", {
  model = list(net="network",movement_matrix="move",dataset="raster")
  #actual_predictions  <- 
  expect_warning(showprediction(model), "showprediction doesn't know how to handle this object.")
})

test_that("showprediction.prediction_model passes correct parameters to show.prediction", {
	movement_predictions = list(net="network",movement_matrix="move",dataset="raster")
  class(movement_predictions)  <- "movement_predictions"
	with_mock(show.prediction = function(x,y,z,...) return(paste(x,y,z,...,sep=',',collaspe=',')),
            actual_predictions  <- showprediction(movement_predictions),
            expect_equal(actual_predictions, "network,raster,move,,"))
})