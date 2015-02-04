\name{showprediction}
\alias{showprediction}

\title{
  Display the movement predictions on a plot
}

\description{
Given a movement model, plot the underlying raster, the configured location points and the predicted movements between locations.
}

\usage{
showprediction(predictionModel)
}

\arguments{
  \item{predictionModel}{a configured prediction model}
}

\seealso{
\code{\link{movementmodel}}, \code{\link{predict}}
}

\examples{

# load kenya raster
data(kenya)

# aggregate to 10km to speed things up
kenya10 <- aggregate(kenya, 10, sum)

# create the prediction model for the aggregate dataset using the fixed parameter radiation model
predictionModel <- movementmodel(dataset=kenya10, min_network_pop = 50000, predictionmodel= 'original radiation', symmetric = TRUE, modelparams = 0.1)

# predict the population movement from the model
predictedMovements = predict(predictionModel)

# visualise the distance matrix
plot(raster(predictedMovements$net$distance_matrix))

# visualise the predicted movements overlaid onto the original raster
showprediction(predictedMovements)

}
