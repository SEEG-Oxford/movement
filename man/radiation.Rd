\name{radiation}
\alias{radiation}

\title{
  Use the radiation model to predict movements across a landscape.
}

\description{
Given a (dense) distance matrix \code{distance} giving the euclidean distances beween all pairs of sites,  a vector of population sizes at these sites \code{population}, the proportion \code{p} of the total population (all sites) commuting, use the function \code{\link{flux}} to predict movement between all sites using the (original) radiation model (Semini et al. 2013).

The model can be sped up by setting \code{minpop} and \code{maxrange}, see \code{flux} for details. If \code{progress = TRUE} a text progress bar is displayed and starting and total times reported.

Note that this is a pretty shitty prototype at the moment and will be re-written in a much more efficient way in the future!
}

\usage{
  radiation(distance, population, p = 1, minpop = 0, maxrange = Inf, progress = TRUE)
}

\arguments{
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{p}{a scalar between 0 and 1 giving the proportion of the total population (all sites) who are moving}
  \item{minpop}{the minimum population size to consider (by default 1, consider all sites)}
  \item{maxrange}{the maximum distance between sites to consider (by default \code{Inf}, consider all sites)}
  \item{progress}{whether to display a progress bar and start and end times - can be useful for big model runs}
}

\value{
A (dense) symmetric matrix giving predicted movements between all sites
}

\seealso{
\code{\link{flux}}
}

\examples{
# to do
}
