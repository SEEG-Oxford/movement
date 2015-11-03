library(movement)
context("Visualisation methods")

test_that("plot.prediction_model passes correct parameters to show.prediction", {
	movement_predictions = list(net="network",movement_matrix="move")
  class(movement_predictions)  <- "movement_predictions"
	with_mock(`movement:::show.prediction` = function(x,y,...) return(paste(x,y,...,sep=',',collaspe=',')),
            actual_predictions  <- plot(movement_predictions),
            expect_equal(actual_predictions, "network,move,,"))
})
