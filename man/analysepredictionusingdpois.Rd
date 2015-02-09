\name{analysepredictionusingdpois}
\alias{analysepredictionusingdpois}

\title{
  Calculate the log likelihood of the prediction given the observed data.
}

\description{
Processes an observed and predicted matrix, strips out the diagonals (which should be zero) and calculates the log likelihood.
}

\usage{
analysepredictionusingdpois(prediction, observed)
}

\arguments{
  \item{prediction}{a square matrix containing the predicted movements between location IDs}
  \item{observed}{a square matrix containing the observed movements between location IDs}
}

\value{
The log likelihood
}

\seealso{
\code{\link{movementmodel}}, \code{\link{attemptoptimisation}}
}
