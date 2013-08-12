\name{gravity.movement}
\alias{gravity.movement}

\title{
  Use the gravity model to predict movements across a landscape.
}

\description{
Given a (dense) distance matrix \code{distance} giving the euclidean
distances beween all pairs of sites,  a vector of population sizes at
these sites \code{population}, and a set of parameters \code{theta}, use
the function \code{\link{flux}} to predict movement between all sites
using a version of the gravity model (Viboud et al. 2006).

The model can be sped up by setting \code{minpop} and \code{maxrange}, see \code{gravity.flux} for details. If \code{progress = TRUE} a text progress bar is displayed and starting and total times reported.

Note that this is a pretty shitty prototype at the moment and will be re-written in a much more efficient way in the future!
}

\usage{
  gravity.movement(distance, population, theta, minpop = 0, maxrange = Inf, progress = TRUE)
}

\arguments{
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{theta}{a vector of parameters in the order scalar, exponent on donor pop, exponent on recipient pop, exonent on distance}
  \item{minpop}{the minimum population size to consider (by default 1, consider all sites)}
  \item{maxrange}{the maximum distance between sites to consider (by default \code{Inf}, consider all sites)}
  \item{progress}{whether to display a progress bar and start and end times - can be useful for big model runs}
}

\value{
A (dense) symmetric matrix giving predicted movements between all sites
}

\seealso{
\code{\link{gravity.flux}}
}

\examples{

# generate random coordinates and populations
n <- 30
coords <- matrix(runif(n * 2), ncol = 2)
pop <- round(runif(n) * 1000)

# calculate the distance between pairs of sites
d <- as.matrix(dist(coords))

# predict movement between them using the radiation model
move <- gravity.movement(d, pop,theta=c(1e-4,0.6,0.3,3))

# plot the points
plot(coords, pch = 16, cex = pop / 500,
     col = 'grey40', axes = FALSE,
     xlab = '', ylab = '')

# and add arrows showing movement
for(i in 2:n) {
  for(j in  (i - 1):n) {
    arrows(coords[i, 1],
           coords[i, 2],
           coords[j, 1],
           coords[j, 2],
           lwd = 2,
           length = 0.1,
           col = rgb(0, 0, 1, move[i, j] / (max(move) + 1)))
  }
}
}
