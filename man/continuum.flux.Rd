\name{continuum.flux}
\alias{continuum.flux}

\title{
  Use the continuum model of Simini et al. (2013) to predict movement between two sites based on population and distance.
}

\description{
Given indices \code{i} and \code{j}, a (dense) distance matrix \code{distance} giving the euclidean distances beween all pairs of sites, a vector of population sizes \code{population} and a set of parameters, use any of three variants of the continuum model (Simini et al. 2013) to predict movements between sites \code{i} and \code{j}.

Specifying which variant of the continuum model to use is achieved by passing one of three character-string arguments to the \code{model}. The three options are 'intervening opportunities', 'radiation with selection', and 'original radiation', which is the default. The mathematical definition of each variant of the model and an explanation of how they are related can be found in Simini et al. (2013).

The first parameter, which is required, is supplied as the first element of the vector \code{theta}. This parameter describes the proportion of all inhabitants in the region commuting. The default is that everyone commutes and thus \code{theta[1]=1}. The second (and last) element of \code{theta} supplies a parameter that is necessary for both the intervening opportunities and radiation with selection variants of the model.

The flux can be calculated either for both directions (by setting \code{symmetric = FALSE}, returning movements for each direction) or for the summed movement between the two (\code{symmetric = TRUE}).

The model can be sped up somewhat by setting \code{minpop} and \code{maxrange}. If either of the two sites has a population lower than \code{minpop} (minimum population size), or if the distance betweent the two sites is greater than \code{maxrange} (the maximum range) it is assumed that no travel occurs between these points.

Note that this function only works for individual site pairs. To calculate movements across a whole landscape, use \code{\link{movement.model}}.
}

\usage{
  continuum.flux(i, j, distance, population, model = 'original radiation', theta = c(1), symmetric = FALSE,
                  minpop = 1, maxrange = Inf)
}

\arguments{
  \item{i, j}{indices for \code{population} and \code{distance} giving the first and second sites}
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{model}{the type of continuum model to use}
  \item{theta}{a vector of parameters in the order: proportion of all inhabitants in the region commuting, parameter required for either the intervening opportunities or radiation with selection model variants.}
  \item{symmetric}{whether to return a single value giving the total predicted movements from i to j and j to i (if \code{TRUE}) or vector of length 2 giving movements from i to j (first element) and from j to i (second element)}
  \item{minpop}{the minimum population size to consider (by default 1, consider all sites)}
  \item{maxrange}{the maximum distance between sites to consider (by default \code{Inf}, consider all sites)}
}

\value{
A vector (of length either 1 or 2) giving the predicted number of people moving between the two sites.
}

\seealso{
\code{\link{movement.predict}}
}

\references{
Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach. \emph{PLoS ONE} 8(3): e60069. \url{http://dx.doi.org/10.1371/journal.pone.0060069}
}

\examples{
# generate random coordinates and populations
n <- 30
coords <- matrix(runif(n * 2), ncol = 2)
pop <- round(runif(n) * 1000)

# calculate the distance between pairs of sites
d <- as.matrix(dist(coords))

# predict movement between sites 3 and 4 using the original radiation model
T_ij <- continuum.flux(3, 4, d, pop)
T_ij
}

