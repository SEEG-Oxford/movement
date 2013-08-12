\name{movement.model}
\alias{movement.model}

\title{
  Use a movement model to predict movements across a landscape.
}

\description{
Given a (dense) distance matrix \code{distance} giving the euclidean distances beween all pairs of sites, a vector of population sizes at these sites \code{population}, use a flux function \code{flux} to predict movement between all sites.



The model can be calculated either for both directions (by setting \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for the summed movement between the two (\code{symmetric = TRUE}, giving a symmetric matrix)). Progress and start and end times can be displayed by setting \code{progress = TRUE} and arguments of the flux functions can specified using the \code{dots} argument.

Note that this is a pretty shitty prototype at the moment and will be re-written in a much more efficient way in the future!
}

\usage{
movement.model(distance, population, flux = radiation.flux, symmetric = FALSE, progress = TRUE, ...)
}

\arguments{
  \item{distance}{a distance matrix giving the euclidean distance between pairs of sites}
  \item{population}{a vector giving the population at all sites}
  \item{flux}{a flux function (currently either \code{\link{radiation.flux}} or \code{\link{gravity.flux}}) used to predict movements}
  \item{symmetric}{whether to calculate symmetric or asymmetric (summed across both directions) movement}
  \item{progress}{whether to display a progress bar and start and end times - can be useful for big model runs}
  \item{dots}{arguments to pass to the flux function}
}

\value{
A (dense) matrix giving predicted movements between all sites
}

\seealso{
\code{\link{radiation.flux}}, \code{\link{gravity.flux}}
}

\examples{

# generate random coordinates and populations
n <- 30
coords <- matrix(runif(n * 2), ncol = 2)
pop <- round(runif(n) * 1000)

# calculate the distance between pairs of sites
d <- as.matrix(dist(coords))

# predict total movement between them using the radiation model
move <- movement.model(d, pop, flux = radiation.flux, symmetric = TRUE,
    theta = 0.1)

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
           length = 0,
           col = rgb(0, 0, 1, move[i, j] / (max(move) + 1)))
  }
}

}
