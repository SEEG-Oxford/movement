\name{flux}
\alias{flux}

\title{
  Use the (original) radiation model to predict movement between two sites
}

\description{
Given indices \code{i} and \code{j}, a vector of population sizes \code{population}, a (dense) distance matrix \code{distance} giving the euclidean distances beween all pairs of sites and the proportion of all inhabitats of the region commuting \code{p} (by default 1, everybody), use the (original) radiation model (Semini et al. 2013) to estimate T_ij, the number of people travelling between sites \code{i} and \code{j}.

The model can be sped up somewhat by setting \code{minpop} and \code{maxrange}. If either of the two sites has a population lower than \code{minpop} (minimum population size), or if the distance betweent the two sites is greater than \code{maxrange} (the maximum range) it is assumed that no travel occurs between these points.

Note that this function only works for individual sites, use \code{\link{radiation}} to calculate movements for multiple populations. Both functions are pretty shitty prototypes at the moment and will be re-written in a much more efficient way in the future!
}

\usage{
  flux(i, j, distance, population, p = 1, minpop = 1, maxrange = Inf)
}

\arguments{
  \item{i, j}{indices for \code{population} and \code{distance} giving the first and second sites}
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{p}{a scalar between 0 and 1 giving the proportion of the total population (all sites) who are moving}
  \item{minpop}{the minimum population size to consider (by default 1, consider all sites)}
  \item{maxrange}{the maximum distance between sites to consider (by default \code{Inf}, consider all sites)}
}

\value{
A scalar value giving the predicted number of people mmoving between the two sites
}

\seealso{
\code{\link{radiation}}
}

\examples{
# to do
}

