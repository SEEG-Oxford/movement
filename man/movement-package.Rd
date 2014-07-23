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
\code{\link{movement.model}}, \code{\link{get.network}}, \code{\link{kenya}}, \code{\link{gravity.flux}}, \code{\link{radiation.flux}}
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

# get the network for pixels with at least 50,000 inhabitants
net <- get.network(kenya10, min = 50000)

# fit a radiation model with fixed parameter
move <- movement.model(distance = net$distance_matrix,
                       population = net$population,
                       flux = radiation.flux,
                       symmetric = TRUE,
                       theta = 0.1)

# visualise the distance matrix
plot(raster(net$distance_matrix))

# plot the raster layer
plot(kenya10)

# rescale the population of those pixels for plotting
size <- 0.1 + 2 * net$population / max(net$population)

# plot the pixels selected, with point size proportional to population size
points(net$coordinates, pch = 16,
      cex = size,
      col = rgb(0, 0, 1, 0.6))

# get the number of locations included
n <- nrow(net$coordinates)

# and add arrows showing movement
for(i in 2:n) {
  for(j in  (i - 1):n) {
    arrows(net$coordinates[i, 1],
           net$coordinates[i, 2],
           net$coordinates[j, 1],
           net$coordinates[j, 2],
           lwd = 4,
           length = 0,
           col = rgb(0, 0, 1, move[i, j] / (max(move) + 0)))
  }
}

}