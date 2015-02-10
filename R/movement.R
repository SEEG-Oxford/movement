require(raster)
require(rgdal)
require(Matrix)
require(tools)

#' Use the continuum model of Simini et al. (2013) to predict movement between
#' two sites based on population and distance.
#'
#' Given indices \code{i} and \code{j}, a (dense) distance matrix
#' \code{distance} giving the euclidean distances beween all pairs of sites, a
#' vector of population sizes \code{population} and a set of parameters, use
#' any of three variants of the continuum model (Simini et al. 2013) to predict
#' movements between sites \code{i} and \code{j}.
#' Specifying which variant of the continuum model to use is achieved by
#' passing one of three character-string arguments to the \code{model}. The
#' three options are 'intervening opportunities', 'radiation with selection',
#' and 'original radiation', which is the default. The mathematical definition
#' of each variant of the model and an explanation of how they are related can
#' be found in Simini et al. (2013).
#' The first parameter, which is required, is supplied as the first element of
#' the vector \code{theta}. This parameter describes the proportion of all
#' inhabitants in the region commuting. The default is that everyone commutes
#' and thus \code{theta[1]=1}. The second (and last) element of \code{theta}
#' supplies a parameter that is necessary for both the intervening
#' opportunities and radiation with selection variants of the model.
#' The flux can be calculated either for both directions (by setting
#' \code{symmetric = FALSE}, returning movements for each direction) or for the
#' summed movement between the two (\code{symmetric = TRUE}).
#' The model can be sped up somewhat by setting \code{minpop} and
#' \code{maxrange}. If either of the two sites has a population lower than
#' \code{minpop} (minimum population size), or if the distance betweent the two
#' sites is greater than \code{maxrange} (the maximum range) it is assumed that
#' no travel occurs between these points.
#' Note that this function only works for individual site pairs. To calculate
#' movements across a whole landscape, use \code{\link{movement.predict}}.
#'
#' @param i Index for \code{population} and \code{distance} giving the first
#' site
#' @param j Index for \code{population} and \code{distance} giving the second
#' site
#' @param distance A distance matrix giving the euclidean distance between
#' pairs of sites
#' @param population A vector giving the population at all sites
#' @param model The type of continuum model to use
#' @param theta A vector of parameters in the order: proportion of all
#' inhabitants in the region commuting, parameter required for either the
#' intervening opportunities or radiation with selection model variants.
#' @param symmetric Whether to return a single value giving the total predicted
#' movements from i to j and j to i (if \code{TRUE}) or vector of length 2
#' giving movements from i to j (first element) and from j to i (second
#' element)
#' @param minpop The minimum population size to consider (by default 1,
#' consider all sites)
#' @param maxrange The maximum distance between sites to consider (by default
#' \code{Inf}, consider all sites)
#' @return A vector (of length either 1 or 2) giving the predicted number of
#' people moving between the two sites.
#'
#' @examples
#' # generate random coordinates and populations
#' n <- 30
#' coords <- matrix(runif(n * 2), ncol = 2)
#' pop <- round(runif(n) * 1000)
#' # calculate the distance between pairs of sites
#' d <- as.matrix(dist(coords))
#' # predict movement between sites 3 and 4 using the original radiation model
#' T_ij <- continuum.flux(3, 4, d, pop)
#' T_ij
#'
#' @seealso \code{\link{movement.predict}}
#'
#' @references
#' Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach.
#' \emph{PLoS ONE} 8(3): e60069.
#' \url{http://dx.doi.org/10.1371/journal.pone.0060069}
continuum.flux <- function(i, j, distance, population,
                           model = 'original radiation',
                           theta = c(1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {

  # get model parameters
  p <- theta[1]
  if(model == 'intervening opportunities'){
    L <- theta[2]
  } else if(model == 'radiation with selection'){
    lambda <- theta[2]
  }

  # get the population sizes $m_i$ and $n_j$
  m_i <- population[i]
  n_j <- population[j]

  # if the population at the centre is below the minimum,
  # return 0 (saves some calculation time)
  if (m_i < minpop | n_j < minpop) {
  # if it's symmetric return one 0
    if (symmetric) return (0)
    # otherwise return two
    else return (c(0, 0))
  }

  # calculate the total number of people commuting from i -
  # the proportion (p) multiplied by the population $m_i$
  T_i <- m_i * p

  # and from j
  T_j <- n_j * p

  # look up $r_{ij}$ - the euclidean distance between $i$ and $j$
  r_ij <- distance[i, j]

  # if it's beyond the maximum range return 0
  if (r_ij > maxrange) {
    # if it's symmetric return one 0
    if (symmetric) return (0)
    # otherwise return two
    else return (c(0, 0))
  }

  # get indices of points within this range
  i_in_radius <- distance[i, ] <= r_ij
  j_in_radius <- distance[j, ] <= r_ij

  # sum the total population in this radius (excluding i & j)

  # calculate $s_{ij}$, the total population in the search radius
  # (excluding $i$ and $j$)

  # which to include in the sum
  i_pop_sum_idx <- i_in_radius
  # not i or j
  i_pop_sum_idx[c(i, j)] <- FALSE
  # get sum
  i_s_ij <- sum(population[i_pop_sum_idx])

  # which to include in the sum
  j_pop_sum_idx <- j_in_radius
  # not i or j
  j_pop_sum_idx[c(i, j)] <- FALSE
  # get sum
  j_s_ij <- sum(population[j_pop_sum_idx])

#   # if the sum is 0 (no populations in that range) return 0 movement
#   if (i_ s_ij == 0) return (0)

  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$
  #
  # this number is calculated as the expectation of a multinomial process
  # of T_i and T_j individuals moving to each other possible site
  # $j$ or $i$ with probabilities P_nam_ij and P_nam_ji, which were
  # derived in Simini et al. (2013)

  m_i_times_n_j <- m_i * n_j
  m_i_plus_n_j <- m_i + n_j

  if(model == 'original radiation'){
    P_nam_ij <- m_i_times_n_j / ((m_i + i_s_ij) * (m_i_plus_n_j + i_s_ij))
    P_nam_ji <- m_i_times_n_j / ((n_j + j_s_ij) * (m_i_plus_n_j + j_s_ij))
  } else if(model == 'intervening opportunities'){
    P_nam_ij <- (exp(-L * (m_i + i_s_ij)) - exp(-L * (m_i_plus_n_j + i_s_ij))) / exp(-L * m_i)
    P_nam_ji <- (exp(-L * (n_j + j_s_ij)) - exp(-L * (m_i_plus_n_j + j_s_ij))) / exp(-L * n_j)
  } else if(model == 'uniform selection'){
    N <- sum(population)
    P_nam_ij <- n_j / (N - m_i)
    P_nam_ji <- m_i / (N - n_j)
  } else if(model == 'radiation with selection'){
    P_nam_ij <-
      ((1 - lambda ^ (m_i + i_s_ij + 1)) / (m_i + i_s_ij + 1) -
       (1 - lambda ^ (m_i_plus_n_j + i_s_ij + 1)) / (m_i_plus_n_j + i_s_ij + 1)) /
      ((1 - lambda ^ (m_i + 1)) / (m_i + 1))
    P_nam_ji <-
      ((1 - lambda ^ (n_j + j_s_ij + 1)) / (n_j + j_s_ij + 1) -
       (1 - lambda ^ (m_i_plus_n_j + j_s_ij + 1)) / (m_i_plus_n_j + j_s_ij + 1)) /
      ((1 - lambda ^ (n_j + 1)) / (n_j + 1))
  }

  T_ij <- T_i * P_nam_ij

  # and in the opposite direction
  T_ji <- T_j * P_nam_ji


  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

#' Use the Viboud et al. 2006 (relatively simple) gravitation model to predict
#' movement between two sites.
#'
#' Given indices \code{i} and \code{j}, a vector of population sizes
#' \code{population}, a (dense) distance matrix \code{distance} giving the
#' euclidean distances beween all pairs of sites, and a set of parameters
#' \code{theta}, to predict movements between sites \code{i} and \code{j}.
#' The flux can be calculated either for both directions (by setting
#'  \code{symmetric = FALSE}, returning movements for each direction) or for
#'  the summed movement between the two (\code{symmetric = TRUE}).
#' The model can be sped up somewhat by setting \code{minpop} and
#' \code{maxrange}. If either of the two sites has a population lower than
#' \code{minpop} (minimum population size), or if the distance betweent the two
#' sites is greater than \code{maxrange} (the maximum range) it is assumed that
#' no travel occurs between these points.
#' Note that this function only works for individual sites, use
#' \code{\link{movement.predict}} to calculate movements for multiple
#' populations.
#'
#' @param i Index for \code{population} and \code{distance} giving the first
#' site
#' @param j Index for \code{population} and \code{distance} giving the second
#' site
#' @param distance A distance matrix giving the euclidean distance between
#' pairs of sites
#' @param population A vector giving the population at all sites
#' @param theta A vector of parameters in the order: scalar, exponent on donor
#' pop, exponent on recipient pop, exponent on distance
#' @param symmetric Whether to return a single value giving the total predicted
#' movements from i to j and j to i (if \code{TRUE}) or vector of length 2
#' giving movements from i to j (first element) and from j to i (second element)
#' @param minpop The minimum population size to consider (by default 1, consider
#' all sites)
#' @param maxrange The maximum distance between sites to consider (by default
#' \code{Inf}, consider all sites)
#' @return A vector (of length either 1 or 2) giving the predicted number of
#' people moving between the two sites.
#'
#' @examples
#' # generate random coordinates and populations
#' n <- 30
#' coords <- matrix(runif(n * 2), ncol = 2)
#' pop <- round(runif(n) * 1000)
#' # calculate the distance between pairs of sites
#' d <- as.matrix(dist(coords))
#' # predict movement between sites 3 and 4 using the radiation model
#' T_ij <- gravity.flux(3, 4, d, pop, theta=c(1e-4,0.6,0.3,3))
#' T_ij
#'
#' @seealso \code{\link{movement.predict}}
#'
#' @references
#' Viboud et al. (2006) Synchrony, Waves, and Spatial Hierarchies in the Spread
#' of Influenza. \emph{Science} \url{http://dx.doi.org/10.1126/science.1125237}
gravity.flux <- function(i, j, distance, population,
                         theta = c(1, 0.6, 0.3, 3),
                         symmetric = FALSE,
                         minpop = 1, maxrange = Inf) {
  # given the indices $i$ and $j$, vector of population sizes
  # 'population', (dense) distance matrix 'distance', vector of parameters
  # 'theta' in the order [scalar, exponent on donor pop, exponent on recipient
  # pop, exponent on distance], calculate $T_{ij}$, the number of people
  # travelling between $i$ and $j$. If the pairwise distance is greater than
  # 'max' it is assumed that no travel occurs between these points. This can
  # speed up the model.

  # get the population sizes $m_i$ and $n_j$
  m_i <- population[i]
  n_j <- population[j]

  # if the population at the centre is below the minimum,
  # return 0 (saves some calculation time)
  m_i[m_i < minpop] <- 0

  # look up $r_{ij}$ - the euclidean distance between $i$ and $j$
  r_ij <- distance[i, j]

  # if it's beyond the maximum range return 0 - this way to vectorize with ease...
  r_ij[r_ij > maxrange] <- 0

  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$ using equation 1 in Viboud et al. (2006)
  T_ij <- theta[1] * (m_i ^ theta[2]) * (n_j ^ theta[3]) / (r_ij ^ theta[4])

  # and the opposite direction
  T_ji <- theta[1] * (n_j ^ theta[2]) * (m_i ^ theta[3]) / (r_ij ^ theta[4])

  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

#' Use a movement model to predict movements across a landscape.
#'
#' Given a (dense) distance matrix \code{distance} giving the euclidean
#' distances between all pairs of sites, a vector of population sizes at these
#' sites \code{population}, use a flux function \code{flux} to predict movement
#' between all sites.
#' The model can be calculated either for both directions (by setting
#' \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for
#' the summed movement between the two (\code{symmetric = TRUE}, giving a
#' symmetric matrix)). Progress and start and end times can be displayed by
#' setting \code{progress = TRUE} and arguments of the flux functions can
#' specified using the \code{dots} argument.
#'
#' @param distance A distance matrix giving the euclidean distance between
#' pairs of sites
#' @param population A vector giving the population at all sites
#' @param flux A flux function (currently either \code{\link{continuum.flux}}
#' or \code{\link{gravity.flux}}) used to predict movements
#' @param symmetric Whether to calculate symmetric or asymmetric (summed
#' across both directions) movement
#' @param progress Whether to display a progress bar and start and end times
#' - can be useful for big model runs
#' @param \dots Arguments to pass to the flux function
#' @return A (dense) matrix giving predicted movements between all sites
#'
#' @examples
#' # generate random coordinates and populations
#' n <- 30
#' coords <- matrix(runif(n * 2), ncol = 2)
#' pop <- round(runif(n) * 1000)
#' # calculate the distance between pairs of sites
#' d <- as.matrix(dist(coords))
#' # predict total movement between them using the radiation model
#' move <- movement.predict(d, pop, flux = continuum.flux, symmetric = TRUE,
#'     theta = 0.1)
#' # plot the points
#' plot(coords, pch = 16, cex = pop / 500,
#'      col = 'grey40', axes = FALSE,
#'      xlab = '', ylab = '')
#' # and add arrows showing movement
#' for(i in 2:n) {
#'   for(j in  (i - 1):n) {
#'     arrows(coords[i, 1],
#'            coords[i, 2],
#'            coords[j, 1],
#'            coords[j, 2],
#'            lwd = 2,
#'            length = 0,
#'            col = rgb(0, 0, 1, move[i, j] / (max(move) + 1)))
#'   }
#' }
#' @seealso \code{\link{gravity.flux}}, \code{\link{continuum.flux}}
movement.predict <- function(distance, population,
                           flux = continuum.flux,
                           symmetric = FALSE,
                           progress = TRUE,
                           ...) {

  # create a movement matrix in which to store movement numbers
  movement <- matrix(NA,
                     nrow = nrow(distance),
                     ncol = ncol(distance))
  # set diagonal to 0
  movement[col(movement) == row(movement)] <- 0

  # get the all $i, j$ pairs
  indices <- which(upper.tri(distance), arr.ind = TRUE)

  # set up optional text progress bar
  if (progress) {
    start <- Sys.time()
    cat(paste('Started processing at',
              start,
              '\nProgress:\n\n'))

    bar <- txtProgressBar(min = 1,
                          max = nrow(indices),
                          style = 3)
  }
  
  # this is where parallelisation should be possible as there is a parApply. It seems that the nodes of the cluster need to know about the flux function.
  # T_ijs <- apply(indices, 1, function(x) flux(i = x[1], j = x[2], distance = distance, population = population, symmetric = symmetric, ...))

  # This can probably be vectorized which should help speed up the population of the movement matrix
  for (idx in 1:nrow(indices)) {
    # for each array index (given as a row of idx), get the pair of nodes
    pair <- indices[idx, ]
    i <- pair[1]
    j <- pair[2]

    # calculate the number of commuters between them
    T_ij <- flux(i = i,
                 j = j,
                 distance = distance,
                 population = population,
                 symmetric = symmetric,
                 ...)

	# and stick it in the results matrix

    # if the symmetric distance was calculated (sum of i to j and j to i)
    # stick it in both triangles
    if (symmetric) {

      movement[i, j] <- movement[j, i] <- T_ij

    } else {

      # otherwise stick one in the upper and one in the the lower
      # (flux returns two numbers in this case)
      # i.e. rows are from (i), columns are to (j)
      movement[i, j] <- T_ij[1]
      movement[j, i] <- T_ij[2]

    }

    if (progress) setTxtProgressBar(bar, idx)

  }

  if (progress) {
    end <- Sys.time()

    cat(paste('\nFinished processing at',
              end,
              '\nTime taken:',
              round(difftime(end,start,units="secs")),
              'seconds\n'))

    close(bar)
  }

  return (movement)
}

#' Extract the necessary components for movement modelling form a population
#' density raster.
#'
#' Given raster map of population density, extract a distance matrix and vector
#' of population sizes for all cells with population density above a minimum
#' threshold. These can be used as network representation of the landscape for
#' use in movement models.
#'
#' @param raster A \code{RasterLayer} object of population density.
#' @param min The minimum population size for inclusion in the network. All
#' cells with populations greater than or equal to \code{min} will be included
#' and other excluded.
#' @param matrix Whether the distance matrix should be returned as a
#' \code{matrix} object (if \code{TRUE}) or as a \code{dist} object (if
#' \code{FALSE}).
#' @return A list with three components:
#'  \item{population }{A vector giving the populations at the cells of
#' interest}
#'  \item{distance_matrix }{A distance matrix (eith of class \code{matrix} or
#' \code{dist}) diving the pairwise euclidean distance between the cells of
#' interest in the units of \code{raster}}
#'  \item{coordinate }{A two-column matrix giving the coordinates of the cells
#' of interest in the units of \code{raster}}
#'
#' @examples
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- aggregate(kenya, 10, sum)
#' # get the network for pixels with at least 50,000 inhabitants
#' net <- get.network(kenya10, min = 50000)
#' # visualise the distance matrix
#' plot(raster(net$distance_matrix))
#' # plot the raster layer
#' plot(kenya10)
#' # rescale the population of those pixels for plotting
#' size <- 0.1 + 2 * net$population / max(net$population)
#' # plot the pixels selected, with point size proportional to population size
#' points(net$coordinates, pch = 16,
#'      cex = size,
#'      col = rgb(0, 0, 1, 0.6))
#'
#' @seealso \code{\link{raster}}, \code{\link{dist}},
#' \code{\link{movement.predict}}
get.network <- function(raster, min = 1, matrix = TRUE) {
  # extract necessary components for movement modelling
  # from a population raster

  # find non-na cells
  keep <- which(!is.na(raster[]))

  # of these, find those above the minimum
  keep <- keep[raster[keep] >= min]

  # get population
  pop <- raster[keep]

  # get coordinates
  coords <- xyFromCell(raster, keep)

  # build distance matrix
  dis <- dist(coords)

  # if we want a matrix, not a 'dist' object convert it
  if (matrix) {
    dis <- as.matrix(dis)
  }

  return (list(population = pop,
               distance_matrix = dis,
               coordinates = coords))

}

# expects a dataframe with origin, long_origin, lat_origin and pop_origin
get.network.fromdataframe <- function(dataframe, min = 1, matrix = TRUE) {
  dataframe <- dataframe[!duplicated(dataframe$origin),]
  pop <- as.numeric(dataframe["pop_origin"]$pop_origin)
  coords <- as.matrix(dataframe[c("long_origin", "lat_origin")])
  coords <- matrix(coords, ncol=2)
  colnames(coords)  <- c("x","y")
  dis <- dist(coords)
  locations <- as.numeric(dataframe["origin"]$origin)

  # if we want a matrix, not a 'dist' object convert it
  if (matrix) {
    dis <- as.matrix(dis)
  }

  return (list(population = pop,
               distance_matrix = dis,
               coordinates = coords,
			   locations = locations))
	
}

# plots the movements within a network onto a raster layer
show.prediction <- function(network, raster_layer, predictedMovements, ...) {
	# visualise the distance matrix
	plot(raster(network$distance_matrix))

	# plot the raster layer
	plot(raster_layer, ...)

	# rescale the population of those pixels for plotting
	size <- 0.1 + 2 * network$population / max(network$population)

	# plot the pixels selected, with point size proportional to population size
	points(network$coordinates, pch = 16,
		  cex = size,
		  col = rgb(0, 0, 1, 0.6))

	# get the number of locations included
	n <- nrow(network$coordinates)

	# and add arrows showing movement
	for(i in 2:n) {
	  for(j in  (i - 1):n) {
		arrows(network$coordinates[i, 1],
			   network$coordinates[i, 2],
			   network$coordinates[j, 1],
			   network$coordinates[j, 2],
			   lwd = 4,
			   length = 0,
			   col = rgb(0, 0, 1, predictedMovements[i, j] / (max(predictedMovements) + 0)))
	  }
	}
}

#' Create a movement model to predict movements across a landscape.
#'
#' This sets up a movement model object which can then be used to predict
#' population movements. It requires a \code{raster} dataset and a number of
#' optional parameters to set up the basic configuration of the prediction
#' model.
#' The model can be calculated either for both directions (by setting
#' \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for
#' the summed movement between the two (\code{symmetric = TRUE}, giving a
#' symmetric matrix)).
#'
#' @param dataset A raster dataset of population data
#' @param min_network_pop The minimum population of a site in order for it to be
#' processed
#' @param predictionmodel The name of a prediction model used to calculated
#' predicted flux between locations. Currently supported prediction models are
#' \code{gravity}, \code{original radiation}, \code{intervening opportunities},
#' \code{radiation with selection} and \code{uniform selection}
#' @param symmetric Whether to calculate symmetric or asymmetric (summed across
#' both directions) movement
#' @param modelparams Model parameter values to pass to the prediction model
#' (such as exponents, scale factors etc.)
#' @return A movement model object which can be used to run flux predictions.
#'
#' @examples
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- aggregate(kenya, 10, sum)
#' # create the prediction model for the aggregate dataset using the fixed parameter radiation model
#' predictionModel <- movementmodel(dataset=kenya10, min_network_pop = 50000, predictionmodel= 'original radiation', symmetric = TRUE, modelparams = 0.1)
#' # predict the population movement from the model
#' predictedMovements = predict(predictionModel)
#' # visualise the distance matrix
#' plot(raster(predictedMovements$net$distance_matrix))
#' # visualise the predicted movements overlaid onto the original raster
#' showprediction(predictedMovements)
#'
#' @seealso \code{\link{predict}}, \code{\link{showprediction}}
movementmodel <- function(dataset, min_network_pop = 50000, predictionmodel = 'original radiation', symmetric = TRUE, modelparams = 0.1) {
	me <- list(
		dataset = dataset,
		min_network_pop = min_network_pop,
		predictionmodel = predictionmodel,
		symmetric = symmetric,
		modelparams = modelparams
		)
	class(me) <- append(class(me), "movementmodel")
	return (me)
}

# base predict function, used to register the method
predict <- function(predictionModel, dataframe, ...) {
	UseMethod("predict", object)
}

# called if predict is run on an unsupported type
predict.default <- function(predictionModel, dataframe, ...) {
	print("predict doesn't know how to handle this object.")
	return (predictionModel)
}

# predict the movements in the network based on the movementmodel provided
# Returns a movementmodel object with the network and prediction fields populated
predict.movementmodel <- function(predictionModel, dataframe = NULL, ...) {
	if(is.null(dataframe)) {
	  net <- get.network(predictionModel$dataset, min = predictionModel$min_network_pop)
	}
	else {
	  net <- get.network.fromdataframe(dataframe = dataframe, min = predictionModel$min_network_pop)
	}
	predictionModel$net = net
	if(predictionModel$predictionmodel == 'gravity'){
		predictionModel$prediction = movement.predict(distance = net$distance_matrix, population = net$population, flux = gravity.flux, symmetric = predictionModel$symmetric, theta = predictionModel$modelparams, ...)
	} else {
		predictionModel$prediction = movement.predict(distance = net$distance_matrix, population = net$population, flux = continuum.flux, symmetric = predictionModel$symmetric, model = predictionModel$predictionmodel, theta = predictionModel$modelparams, ...)
	}
	
	return (predictionModel)
}

# base showprediction function, used to register the method
showprediction <- function(predictionModel, ...) {
	UseMethod("showprediction", predictionModel)
}

# called if showprediction is run on an unsupported type
showprediction.default <- function(predictionModel, ...) {
	print("showprediction doesn't know how to handle this object.")
	return (predictionModel)
}

# Show a plot of the predicted movementmodel. Shows the underlying raster plot in addition to the predicted movements.
showprediction.movementmodel <- function(predictionModel, ...) {
	network <- predictionModel$net
	move <- predictionModel$prediction
	raster <- predictionModel$dataset
	show.prediction(network, raster, move, ...)
}

# utility function to rasterize a shape file an discard unnecessary layers
rasterizeShapeFile <- function(filename, keeplist)  {
	# load the shapefile into a SpatialPolygonsDataFrame
	dsn = dirname(filename)
	filename = basename(filename)
	layer = file_path_sans_ext(filename)
	shapeObject = readOGR(dsn = dsn, layer = layer)
	shapeObject <- shapeObject[keeplist]
	
	# get the extents of the dataframe
	extents = extent(shapeObject)
	xmin = extents@xmin
	xmax = extents@xmax
	ymin = extents@ymin
	ymax = extents@ymax
	
	# set up a raster template to use in rasterize()
	ext <- extent (xmin, xmax, ymin, ymax)
	xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
	n <- 5
	r <- raster(ext, ncol=xy[1]*50, nrow=xy[2]*50)
	
	rr <- rasterize(shapeObject, r)
	## create a population only rasterlayer (i.e. remove the RAT table)
	rr <- deratify(rr, keeplist)
	return (rr)
}

createobservedmatrixfromcsv <- function(filename, origincolname, destcolname, valcolname) {
	data <- read.csv(file=filename,header=TRUE,sep=",")
	nrows = length(unique(data[origincolname])[,1])
	ncols = length(unique(data[destcolname])[,1])
	
	# mapping locationids to matrix row or column ids to cope with missing locationids
	origins = sort(as.numeric(unique(data[origincolname])[,1]))
	destinations = sort(as.numeric(unique(data[destcolname])[,1]))
	
	sparseMatrix <- matrix(nrow = nrows, ncol = ncols)
	for (idx in 1:length(data$X)) {
		sparseMatrix[match(data[idx,origincolname],origins),match(data[idx,destcolname],destinations)] = data[idx,valcolname]
	}
	
	# set any remaining NAs to 0
	sparseMatrix[is.na(sparseMatrix)] <- 0
	
	return (sparseMatrix)
}

createpopulationfromcsv <- function(filename) {
	data <- read.csv(file=filename,header=TRUE,sep=",")
	
	return (data)
}

analysepredictionusingdpois <- function(prediction, observed) {	
	observed = c(observed[upper.tri(observed)], observed[lower.tri(observed)])
	predicted = c(prediction$prediction[upper.tri(prediction$prediction)], prediction$prediction[lower.tri(prediction$prediction)])
	
	retval <- sum(dpois(observed, predicted, log = TRUE)) * -2;
	if(is.nan(retval)) {
		cat(paste('Warning: Likelihood was NaN, changing to Max Value to allow simulation to continue\n'))
		retval <- .Machine$double.xmax
	}

	return (retval)
}

# wrapper around our real simulation which returns a log likelihood which can be used by optim
fittingwrapper <- function(par, predictionModel, observedmatrix, populationdata, ...) {
	cat(paste('========\n'))
	cat(paste('Parameters: ',
              par,
              '\n'))
	# set the initial model params to par
	predictionModel$modelparams = par
	predictedResults <- predict.movementmodel(predictionModel, populationdata, ...)
	loglikelihood <- analysepredictionusingdpois(predictedResults, observedmatrix)
	cat(paste('Log Likelihood: ',
              loglikelihood,
              '\n'))
	return (loglikelihood)
}

# simple call to optim passing in a prediction model, the associated population data and the observed data to use in the log likelihood calculations
attemptoptimisation <- function(predictionModel, populationdata, observedmatrix, ...) {
	# run optimisation on the prediction model using the BFGS method. The initial parameters set in the prediction model are used as the initial par value for optimisation
	optim(predictionModel$modelparams, fittingwrapper, method="BFGS", predictionModel = predictionModel, observedmatrix = observedmatrix, populationdata = populationdata, ...)
	#control = list(maxit = 100, temp = c(0.01,0.01,0.01,0.01), parscale = c(0.1,0.1,0.1,0.1))
}

## example code for creating the population data and observed matrix
##
##	observedmatrix <- createobservedmatrixfromcsv("../SEEG/France/odmatrix.csv", "origin", "destination", "movement")
##	populationdata <- createpopulationfromcsv("../SEEG/France/odmatrix.csv")
##