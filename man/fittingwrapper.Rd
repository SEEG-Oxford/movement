\name{fittingwrapper}
\alias{fittingwrapper}

\title{
Internal helper function for optimisation
}
\description{
Calls the model prediction code and then calculates a log likelihood metric used as the \code{\link{optim}} minimisation value
}
\usage{
fittingwrapper(par, predictionModel, observedmatrix, populationdata)
}

\arguments{
  \item{par}{theta values for the flux function}
  \item{predictionModel}{The prediction model being optimised}
  \item{observedmatrix}{A matrix containing the observed population movements}
  \item{populationdata}{A dataframe containing population coordinate data}
}

\value{
The log likelihood of the prediction given the observed data.
}

\seealso{
}