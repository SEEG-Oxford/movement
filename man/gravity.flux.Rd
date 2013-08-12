\name{gravity.flux}
\alias{gravity.flux}

\title{
  Use the Viboud et al. 2006 (relatively simple) gravitation model to predict movement between two sites
}

\description{
Given indices \code{i} and \code{j}, a vector of population sizes
\code{population}, a (dense) distance matrix \code{distance} giving the
euclidean distances beween all pairs of sites, and a set of parameters \code{theta}, to predict movements between sites \code{i} and \code{j}.

The flux can be calculated either for both directions (by setting \code{symmetric = FALSE}, returning movements for each direction) or for the summed movement between the two (\code{symmetric = TRUE}).

The model can be sped up somewhat by setting \code{minpop} and \code{maxrange}. If either of the two sites has a population lower than \code{minpop} (minimum population size), or if the distance betweent the two sites is greater than \code{maxrange} (the maximum range) it is assumed that no travel occurs between these points.

Note that this function only works for individual sites, use \code{\link{movement.model}} to calculate movements for multiple populations. Both functions are pretty shitty prototypes at the moment and will be re-written in a much more efficient way in the future!
}

\usage{
  gravity.flux(i, j, distance, population, theta = c(1, 0.6, 0.3, 3), symmetric = FALSE,
                minpop = 1, maxrange = Inf)
}

\arguments{
  \item{i, j}{indices for \code{population} and \code{distance} giving the first and second sites}
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{theta}{a vector of parameters in the order: scalar, exponent on donor pop, exponent on recipient pop, exponent on distance}
    \item{symmetric}{whether to return a single value giving the total predicted movements from i to j and j to i (if \code{TRUE}) or vector of length 2 giving movements from i to j (first element) and from j to i (second element)}
  \item{minpop}{the minimum population size to consider (by default 1, consider all sites)}
  \item{maxrange}{the maximum distance between sites to consider (by default \code{Inf}, consider all sites)}
}

\value{
A vector (of length either 1 or 2) giving the predicted number of people moving between the two sites.
}

\references{
Viboud et al. (2006) Synchrony, Waves, and Spatial Hierarchies in the Spread of Influenza. \emph{Science} \url{http://dx.doi.org/10.1126/science.1125237}
}

\seealso{
\code{\link{movement.model}}
}

\examples{
# generate random coordinates and populations
n <- 30
coords <- matrix(runif(n * 2), ncol = 2)
pop <- round(runif(n) * 1000)

# calculate the distance between pairs of sites
d <- as.matrix(dist(coords))

# predict movement between sites 3 and 4 using the radiation model
T_ij <- gravity.flux(3, 4, d, pop, theta=c(1e-4,0.6,0.3,3))
T_ij
}

