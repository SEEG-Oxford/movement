\name{attemptoptimisation}
\alias{attemptoptimisation}

\title{
  Attempt to optimise the parameters of a given movement model based on log likelihoods against observed data
}

\description{
Runs the optim function using the BFGS optimisation method to try and optimise the parameters of the given prediction model.
}

\usage{
attemptoptimisation(predictionModel, populationdata, observedmatrix)
}

\arguments{
  \item{predictionModel}{a configured prediction model}
  \item{populationdata}{a dataframe containing population data linked to the IDs in the predictionModel raster. Requires 4 columns named \code{origin}, \code{pop_origin}, \code{long_origin}, \code{lat_origin}}
  \item{observedmatrix}{a matrix containing observed population movements. Row and column numbers correspond to the indexes of a sorted list of the origins and destinations used in populationdata. Values are the actual population movements}
}

\value{
See \code{\link{optim}}
}

\seealso{
\code{\link{movementmodel}}, \code{\link{showprediction}}
}

\examples{

# convert france shapefile into raster, keeping layer ID_3
france <- rasterizeShapeFile('france.shp', c('ID_3'))

# create the prediction model for the dataset using the radiation with selection model
predictionModel <- movementmodel(dataset=france, min_network_pop = 50000, predictionmodel= 'radiation with selection', symmetric = TRUE, modelparams = c(0.999, 0.998))

# load the observed movement data into a matrix
observedmatrix <- createobservedmatrixfromcsv("movementmatrix.csv", "origin", "destination", "movement")

# load the population data into a dataframe
populationdata <- createpopulationfromcsv("movementmatrix.csv")

# attempt to optimise the model
attemptoptimisation(predictionModel, populationdata, observedmatrix)
}
