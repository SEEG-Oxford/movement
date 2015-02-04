\name{movementmodel}
\alias{movementmodel}

\title{
  Create a movement model to predict movements across a landscape.
}

\description{
This sets up a movement model object which can then be used to predict population movements. It requires a \code{raster} dataset and a number of optional parameters to set up the basic configuration of the prediction model.

The model can be calculated either for both directions (by setting \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for the summed movement between the two (\code{symmetric = TRUE}, giving a symmetric matrix)).
}

\usage{
movementmodel(dataset, min_network_pop = 50000, predictionmodel = 'original radiation', symmetric = TRUE, modelparams = 0.1)
}

\arguments{
  \item{dataset}{a raster dataset of population data}
  \item{min_network_pop}{the minimum population of a site in order for it to be processed}
  \item{predictionmodel}{the name of a prediction model used to calculated predicted flux between locations}
  \item{symmetric}{whether to calculate symmetric or asymmetric (summed across both directions) movement}
  \item{modelparams}{model parameter values to pass to the prediction model (such as exponents, scale factors etc.)}
}

\value{
A movement model object which can be used to run flux predictions
}

\seealso{
\code{\link{predict}}, \code{\link{showprediction}}
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
