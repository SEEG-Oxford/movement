\name{movement-package}
\alias{movement-package}
\alias{movement}
\docType{package}
\title{
Functions to analyse and model movement data for epidemiology.
}
\description{
Movement of humans and animals has a crucial role in the epidemiology of a number of diseases. Movement data is increasingly available to epidemiologists and its incorporation in models and maps of disease is increasingly popular. This package is a collaborative effort to improve our ability to analyses movement data and to build and apply epidemiological movement models.

}
\details{
\tabular{ll}{
Package: \tab movement\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-08-08\cr
License: \tab GPL (>=2)\cr
}

}


\seealso{
\code{\link{movement.predict}}, \code{\link{get.network}}, \code{\link{kenya}}, \code{\link{gravity.flux}}, \code{\link{continuum.flux}}
}

\author{
Nick Golding + others (to be added)

Maintainer: Nick Golding <nick.golding@zoo.ox.ac.uk>
}

\examples{

# load kenya raster
data(kenya)

# aggregate to 10km to speed things up
kenya10 <- aggregate(kenya, 10, sum)

# create the prediction model for the aggregate dataset using the fixed parameter radiation model
predictionModel <- MovementModel(dataset=kenya10, min_network_pop = 50000, predictionmodel= 'original radiation', symmetric = TRUE, modelparams = 0.1)

# predict the population movement from the model
predictedMovements = Predict(predictionModel)

# visualise the distance matrix
plot(raster(predictedMovements$net$distance_matrix))

# visualise the predicted movements overlaid onto the original raster
ShowPrediction(predictedMovements)

}
