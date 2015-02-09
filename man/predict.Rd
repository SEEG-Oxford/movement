\name{predict}
\alias{predict}

\title{
  Create a movement prediction based on a configured movement model
}

\description{
Given a movement model, use the configured distances and flux function to predict movement between all sites.

Any extra arguments of the flux functions can specified using the \code{dots} argument.
}

\usage{
predict(predictionModel, dataframe, \dots)
}

\arguments{
  \item{predictionModel}{a configured prediction model}
  \item{dataframe}{an optional data frame containing population data}
  \item{\dots}{extra arguments to pass to the flux function}
}

\value{
A \code{movementmodel} containing a (dense) matrix giving predicted movements between all sites
}

\seealso{
\code{\link{movementmodel}}, \code{\link{showprediction}}
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
