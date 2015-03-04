require(raster)
require(rgdal)
require(Matrix)
require(tools)

library(foreach)
library(doParallel)

###############################################################################
# Main interface methods                                                      #
###############################################################################

#' Create an optimised movement model
#'
#' Uses the \code{\link{optim}} method to create an optimised model of
#' population movements.
#' @param locations A vector containing populations
#' @param coords A data frame containing coordinates of the \code{locations}
#' @param population A vector containing populations of the \code{locations}
#' @param movement_matrix A square matrix containing the observed population
#' movements between \code{locations}
#' @param model The name of the movement model to use. Currently supported
#' models are \code{original radiation}, \code{radiation with selection},
#' \code{uniform selection}, \code{intervening opportunities},
#' \code{gravity}
#' @param \dots Extra parameters to be passed to the prediction code. A useful
#' parameter to note is \code{threadCount} which determines how many threads
#' to use when optimising the mode. More is not necessarily better as there is
#' an inherent overhead in creating new threads.
#' @return An \code{optimisedmodel} object containing the training results,
#' and the optimisation results. This can then be used by 
#' \code{\link{predict}} to generate predictions on new data.
#'
#' @seealso \code{\link{predict}}, \code{\link{as.locationdataframe}},
#' \code{\link{as.movementmatrix}}
#' @note The most likely format of the location data will be as a single
#' \code{data.frame} of this form:
#' #   location pop        lat        lon
#' # 1        a 100 0.07826932 0.13612404
#' # 2        b  88 0.12114115 0.58984725
#' # 3        c 100 0.07126503 0.19544754
#' # 4        d 113 0.97817937 0.22771625
#' # 5        e 107 0.87233335 0.06695538
#' This can be extracted from a larger dataframe with
#' \code{\link{as.locationdataframe}}
#' The \code{movement_matrix} can be extracted from a list of movements
#' using \code{\link{as.movementmatrix}}
movement <- function(locations, coords, population, movement_matrix, model, ...) {
	# create the correct params object with (hopefully sane) default values
	if(model == "original radiation" || model == "uniform selection") {
		params <- c(theta=0.9)
		upper <- c(Inf)
		lower <- c(0)
	} else if(model == "radiation with selection") {
		params <- c(theta=0.1,lambda=0.2)
		upper <- c(Inf, 1)
		lower <- c(0, 0)
	} else if(model == "intervening opportunities") {
		params <- c(theta=0.001, L=0.00001)
		upper <- c(Inf, Inf)
		lower <- c(1e-20, 1e-05)
	} else if(model == "gravity") {
		params <- c(theta=0.01, alpha=0.06, beta=0.03, gamma=0.01)
		upper <- c(Inf, Inf, Inf, Inf)
		lower <- c(1e-20, -Inf, -Inf, -Inf)
	} else {
		cat("Error: Unknown model type given\n")
		return ()
	}
	
	# statistics
	# http://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r
	nobs <- nrow(movement_matrix) * ncol(movement_matrix) - nrow(movement_matrix) # all values in the movement_matrix except the diagonal
	nulldf <- nobs # no predictors for null degrees of freedom
	
	# create the prediction model
	predictionModel <- movementmodel(dataset=NULL, min_network_pop=50000, predictionmodel=model, symmetric=FALSE, modelparams=params)
		
	# assemble a population_data data.frame for predict.movementmodel to use
	population_data <- data.frame(origin=locations,pop_origin=population,long_origin=coords[,1],lat_origin=coords[,2])
	
	# attempt to parameterise the model using optim
	optimresults <- attemptoptimisation(predictionModel, population_data, movement_matrix, progress=FALSE, hessian=TRUE, upper=upper, lower=lower, ...) #, upper=upper, lower=lower
	predictionModel$modelparams = optimresults$par
	
	# populate the training results (so we can see the end result)
	training_results <- predict.movementmodel(predictionModel, population_data, progress=FALSE)
	training_results$modelparams <- optimresults$par
	cat("Training complete.\n")
	dimnames(training_results$prediction) <- dimnames(movement_matrix)
	me <- list(optimisationresults = optimresults,
				trainingresults = training_results,
				coefficients = optimresults$par,
				df.null = nulldf, # not checked
				df.residual = nulldf - length(optimresults$value), # not checked
				null.deviance = analysepredictionusingdpois(training_results, c(0,0)), # intercept only model, this is clearly wrong
				deviance = optimresults$value, # -2* log likelihood, which is what we are optimising on anyway
				aic = optimresults$value + 2 * length(optimresults$value)) # deviance + (2* number of params)
	class(me) <- "optimisedmodel"
	return (me)
}

#' Predict population movements from a population input
#'
#' Use a trained \code{optimisedmodel} object to predict population movements
#' given eiether a RasterLayer containing a single population layer, or a
#' data.frame containing population and location data formatted as:
#' #   location pop        lat        lon
#' # 1        a 100 0.07826932 0.13612404
#' # 2        b  88 0.12114115 0.58984725
#' # 3        c 100 0.07126503 0.19544754
#' # 4        d 113 0.97817937 0.22771625
#'# 5        e 107 0.87233335 0.06695538
#' @param model An \code{optimisedmodel} object containing the trained model
#' @param input A \code{RasterLayer} object containing a single population
#' attribute, or a data.frame containing population and location data
#' @return A list containing a location dataframe from the input, and a matrix
#' containing the predicted population movements.
#'
#' @seealso \code{\link{movement}}, \code{\link{predict.movementmodel}}
predict.optimisedmodel <- function(model, input) {
	m <- model$trainingresults
	m$dataset <- input
	if(is(input, "RasterLayer")) {
		prediction <- predict.movementmodel(m)
		df <- data.frame(location=prediction$net$locations, pop=prediction$net$population, prediction$net$coordinates)
		return (list(
			df_locations = df,
			movement_matrix = prediction$prediction))
	} else if (is(input, "data.frame")) {
		prediction <- predict.movementmodel(m, input)
		df <- data.frame(location=prediction$net$locations, pop=prediction$net$population, prediction$net$coordinates)
		return (list(
			df_locations = df,
			movement_matrix = prediction$prediction))
	} else {
		cat('Error: Expected parameter `input` to be either a RasterLayer or a data.frame\n')
	}
}

print.optimisedmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
	cat(paste('Model:  ', x$trainingresults$predictionmodel, '\n\n'))
    if(length(coef(x))) {
        cat("Coefficients")
        cat(":\n")
        print.default(format(x$coefficients, digits = digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
    cat("Null Deviance:	   ", x$null.deviance,
	"\nResidual Deviance: ", x$deviance,
	"\tAIC:", x$aic)
    cat("\n")
    invisible(x)
}

summary.optimisedmodel <- function(x) {
	coef.p <- x$trainingresults$modelparams
	dn <- c("Estimate", "Std. Error")
	stderrors <- sqrt(abs(diag(solve(m$optimisationresults$hessian)))) # need to plug this into the coef table
	ans <- list(
		model = x$trainingresults$predictionmodel,
		deviance.resid = 1,
		coefficients = coef.p,
		nulldeviance = x$null.deviance,
		residdeviance = x$deviance,
		aic = x$aic,
		df.null = x$df.null,
		df.residual = x$df.residual,
		stderrors = stderrors)
	class(ans) <- "summary.optimisedmodel"
	return (ans)
}

print.summary.optimisedmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
	cat(paste('Model:  ', x$model, '\n\n'))
	cat(paste('Deviance Residuals:  ', x$deviance.resid, '\n\n'))
	if(length(coef(x))) {
        cat("Coefficients")
        cat(":\n")
        print.default(format(x$coefficients, digits = digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
	cat(paste('Null Deviance:     ', x$nulldeviance, 'on', x$df.null, 'degrees of freedom\n'))
	cat(paste('Residual Deviance: ', x$residdeviance, 'on', x$df.residual, 'degrees of freedom\n'))
	cat(paste('AIC:  ', x$aic, '\n'))
}

###############################################################################
# Model definition and prediction methods                                     #
###############################################################################

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
						   threadCount = 1,
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
  
  #cpuCount <- threadCount
  #cl <- makeCluster(cpuCount, outfile="output.log")
  #registerDoParallel(cl)
  #chunkSize <- ceiling(nrow(indices) / cpuCount)
  
  #T_ijs <- foreach(idx=icount(cpuCount), .combine=cbind) %dopar% {
	#startIndex <- ((idx - 1) * chunkSize) + 1
	#endIndex <- min(c((idx * chunkSize), nrow(indices)))
	#iT_ijs <- apply(indices[startIndex:endIndex,], 1, function(x) flux(i = x[1], j = x[2], distance = distance, population = population, symmetric = symmetric, ...))
	#return (iT_ijs)
  #}
  
  #T_ijs <- matrix(T_ijs, ncol=2)
  
  #stopCluster(cl)
  
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

###############################################################################
# Prediction visualisation methods                                            #
###############################################################################

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

#' Display the movement predictions on a plot
#'
#' @param predictionModel A configured prediction model
#' @param \dots Extra parameters to pass to plot
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
#' @seealso \code{\link{movementmodel}}, \code{\link{predict}}
showprediction <- function(predictionModel, ...) {
	UseMethod("showprediction", predictionModel)
}

#' @describeIn showprediction Default action for showprediction
showprediction.default <- function(predictionModel, ...) {
	print("showprediction doesn't know how to handle this object.")
	return (predictionModel)
}

#' @describeIn showprediction Given a movement model, plot the underlying
#' raster, the configured location points and the predicted movements
#' between locations.
showprediction.movementmodel <- function(predictionModel, ...) {
	network <- predictionModel$net
	move <- predictionModel$prediction
	raster <- predictionModel$dataset
	show.prediction(network, raster, move, ...)
}

###############################################################################
# Internal prediction and optimisation methods                                #
###############################################################################

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
               coordinates = coords,
			   locations = keep))

}

# expects a dataframe with origin, long_origin, lat_origin and pop_origin
get.network.fromdataframe <- function(dataframe, min = 1, matrix = TRUE) {
  dataframe <- dataframe[!duplicated(dataframe$origin),]
  pop <- as.numeric(dataframe["pop_origin"]$pop_origin)
  coords <- as.matrix(dataframe[c("long_origin", "lat_origin")])
  coords <- matrix(coords, ncol=2)
  colnames(coords)  <- c("x","y")
  dis <- dist(coords)
  locations <- dataframe["origin"]$origin

  # if we want a matrix, not a 'dist' object convert it
  if (matrix) {
    dis <- as.matrix(dis)
  }

  return (list(population = pop,
               distance_matrix = dis,
               coordinates = coords,
			   locations = locations))
	
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

#' Create a movement prediction based on a configured movement model
#'
#' @param predictionModel A configured prediction model
#' @param dataframe An optional data frame containing population data
#' @param \dots eExtra arguments to pass to the flux function
#' @return A \code{movementmodel} containing a (dense) matrix giving predicted
#' movements between all sites.
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
#' @seealso \code{\link{movementmodel}}, \code{\link{showprediction}}
predict <- function(predictionModel, dataframe, ...) {
	UseMethod("predict", predictionModel)
}

#' @describeIn predict Default action for predict
predict.default <- function(predictionModel, dataframe, ...) {
	print("predict doesn't know how to handle this object.")
	return (predictionModel)
}

#' @describeIn predict Given a movement model, use the configured distances and
#' flux function to predict movement between all sites.
#' Any extra arguments of the flux functions can specified using the
#' \code{dots} argument.
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

#' Calculate the log likelihood of the prediction given the observed data.
#'
#' Processes an observed and predicted matrix, strips out the diagonals (which
#' should be zero) and calculates the log likelihood.
#'
#' @param prediction A square matrix containing the predicted movements between location IDs
#' @param observed A square matrix containing the observed movements between location IDs
#' @return The log likelihood
#'
#' @seealso \code{\link{movementmodel}}, \code{\link{attemptoptimisation}}
analysepredictionusingdpois <- function(prediction, observed) {	
	observed = c(observed[upper.tri(observed)], observed[lower.tri(observed)])
	predicted = c(prediction$prediction[upper.tri(prediction$prediction)], prediction$prediction[lower.tri(prediction$prediction)])
	
	retval <- sum(dpois(observed, predicted, log = TRUE)) * -2;
	#if(is.nan(retval)) {
	#	cat(paste('Warning: Likelihood was NaN, changing to Max Value to allow simulation to continue\n'))
	#	retval <- .Machine$double.xmax
	#}

	return (retval)
}

#' Internal helper function for optimisation
#'
#' Calls the model prediction code and then calculates a log likelihood metric
#' used as the \code{\link{optim}} minimisation value
#'
#' @param par theta values for the flux function
#' @param predictionModel The prediction model being optimised
#' @param observedmatrix A matrix containing the observed population movements
#' @param populationdata A dataframe containing population coordinate data
#' @param \dots Parameters passed to \code{\link{movement.predict}}
#' @return The log likelihood of the prediction given the observed data.
fittingwrapper <- function(par, predictionModel, observedmatrix, populationdata, ...) {
	# set the initial model params to par
	predictionModel$modelparams = par
	predictedResults <- predict.movementmodel(predictionModel, populationdata, ...)
	loglikelihood <- analysepredictionusingdpois(predictedResults, observedmatrix)
	return (loglikelihood)
}

#' Attempt to optimise the parameters of a given movement model based on log
#' likelihoods against observed data.
#'
#' Runs the optim function using the BFGS optimisation method to try and
#' optimise the parameters of the given prediction model.
#'
#' @param predictionModel A configured prediction model
#' @param populationdata A dataframe containing population data linked to the
#' IDs in the predictionModel raster. Requires 4 columns named \code{origin},
#' \code{pop_origin}, \code{long_origin}, \code{lat_origin}
#' @param observedmatrix A matrix containing observed population movements. Row
#' and column numbers correspond to the indexes of a sorted list of the origins
#' and destinations used in populationdata. Values are the actual population
#' movements.
#' @param \dots Parameters passed to \code{\link{movement.predict}}
#' @return See \code{\link{optim}}
#'
#' @examples
#' # convert france shapefile into raster, keeping layer ID_3
#' france <- rasterizeShapeFile('france.shp', c('ID_3'))
#' # create the prediction model for the dataset using the radiation with
#' # selection model
#' predictionModel <- movementmodel(dataset=france, min_network_pop = 50000,
#' predictionmodel= 'radiation with selection', symmetric = TRUE, modelparams
#' = c(0.999, 0.998))
#' # load the observed movement data into a matrix
#' observedmatrix <- createobservedmatrixfromcsv("movementmatrix.csv",
#' "origin", "destination", "movement")
#' # load the population data into a dataframe
#' populationdata <- createpopulationfromcsv("movementmatrix.csv")
#' # attempt to optimise the model
#' attemptoptimisation(predictionModel, populationdata, observedmatrix)
#'
#' @seealso \code{\link{movementmodel}},
#' \code{\link{createobservedmatrixfromcsv}},
#' \code{\link{createpopulationfromcsv}},
#' \code{\link{analysepredictionusingdpois}}
attemptoptimisation <- function(predictionModel, populationdata, observedmatrix, ...) {
	# run optimisation on the prediction model using the BFGS method. The initial parameters set in the prediction model are used as the initial par value for optimisation
	optim(predictionModel$modelparams, fittingwrapper, method="L-BFGS-B", predictionModel = predictionModel, observedmatrix = observedmatrix, populationdata = populationdata, ...)
}

###############################################################################
# Data manipulation helper functions                                          #
###############################################################################

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

#' Create a matrix of observed population movements
#'
#' Reads a correctly formatted csv file and creates a matrix containing
#' observed movement between different indexed locations
#'
#' @param filename File path of the csv file to process
#' @param origincolname The name of the column containing origin IDs
#' @param destcolname The name of the column containing destination IDs
#' @param valcolname The name of the column containing population movement
#' values
#' @return A matrix containing observed population movements. Row and column
#' numbers correspond to the indexes of a sorted list of the origins found in
#' the csv file. Values are the actual population movements.
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

#' Create a data frame of populations at particular coordinates
#'
#' Reads a correctly formatted csv file and creates a dataframe
#'
#' @param filename File path of the csv file to process
#' @return A dataframe containing csv data.
createpopulationfromcsv <- function(filename) {
	data <- read.csv(file=filename,header=TRUE,sep=",")
	
	return (data)
}

#' Convert a data.frame into a movement matrix
#'
#' Takes a dataframe listing movements between different locations and converts
#' it into a square matrix using the same location ids.
#' The dataframe does not need to include the a->a transitions as these are
#' automatically filled with zero if missing. This results in a zero diagonal
#' through the matrix.
#' @param dataframe A data.frame of the format
#' #   origin destination movement
#' # 1      a           b       10
#' # 2      a           c        8
#' # 3      a           d       10
#' # 4      a           e       11
#' # 5      a           f        8
#' (truncated)
#' @return A square matrix
as.movementmatrix <- function(dataframe) {
	nrows <- length(unique(dataframe[1])[,])
	ncols <- length(unique(dataframe[2])[,])
	if(nrows != ncols) {
		stop ("Error: Expected a square matrix!")
		return (NULL)
	}
	
	mat <- matrix(ncol = ncols, nrow = nrows, dimnames = list(sort(unique(dataframe[1])[,]),sort(unique(dataframe[2])[,])))
	for(idx in 1:nrow(dataframe)) {
		mat[as.character(dataframe[idx,2]),as.character(dataframe[idx,1])] <- dataframe[idx,3]
	}
	
	mat[is.na(mat)] <- 0
	
	return (mat)
}

as.locationdataframe <- function(dataframe, ...) {
	UseMethod("as.locationdataframe", dataframe)
}

#' Convert a merged data.frame into a single location data.frame
#'
#' Takes a data.frame containing location and population data and converts it
#' into a single data.frame containing location data only.
#' @param dataframe A data.frame of the format
#' #   origin destination movement pop_origin  pop_destination  
#' # 1      a           b       10        100               88
#' # 2      a           c        8        100              100
#' # 3      a           d       10        100              113
#' # 4      a           e       11        100              107
#' # 5      a           f        8        100               67
#' #   lat_origin long_origin  lat_destination long_destination
#' #   0.07826932  0.13612404       0.12114115       0.58984725
#' #   0.07826932  0.13612404       0.07126503       0.19544754
#' #   0.07826932  0.13612404       0.97817937       0.22771625
#' #   0.07826932  0.13612404       0.87233335       0.06695538
#' #   0.07826932  0.13612404       0.23157835       0.19573021
#' @return A data.frame containing location data of the format
#' #   location pop        lat        lon
#' # 1        a 100 0.07826932 0.13612404
#' # 2        b  88 0.12114115 0.58984725
#' # 3        c 100 0.07126503 0.19544754
#' # 4        d 113 0.97817937 0.22771625
#' # 5        e 107 0.87233335 0.06695538
as.locationdataframe.data.frame <- function(dataframe) {
	  dataframe <- dataframe[!duplicated(dataframe$origin),]
	  pop <- as.numeric(dataframe["pop_origin"]$pop_origin)
	  lat <- as.numeric(dataframe["lat_origin"]$lat_origin)
	  long <- as.numeric(dataframe["long_origin"]$long_origin)
	  locations <- as.numeric(dataframe["origin"]$origin)

	  return (data.frame(location = locations,
				   pop = pop,
				   lat = lat,
				   lon = long))
}

# use region data downloaded from http://www.gadm.org/country along with a world population raster
# make sure it is cropped to the correct region first using raster::crop
# for portugal, this works: crop(gadm, extent(-10, -6.189142, 30, 42.154232))
# portugal gadm is missing 2 municipalities (Tavira and Guimaraes): http://www.igeo.pt/DadosAbertos/Listagem.aspx#
as.locationdataframe <- function(gadm, populationraster) {
	result <- data.frame(simplifytext(gadm$NAME_2),gadm$ID_2,extract(world,gadm, fun=sum),coordinates(gadm))
	colnames(result) <- c("name", "location", "pop", "lon", "lat")
	return (result)
}

simplifytext <- function(string) {
	return (gsub("\\s", "_", toupper(iconv(string, from='UTF-8', to='ASCII//TRANSLIT'))))
}

correlateregions <- function(dataframe, regionlist, movementdata) {
	allnames <- as.vector(dataframe$name)
	datanames <- as.vector(regionlist$V2)
	
	# work out which regions in the regionlist are present in the location dataframe	
	datainall <- data.frame(datanames, datanames %in% allnames)
	colnames(datainall) <- c("name", "inlist")
	datapresentinall <- which(datainall$inlist == TRUE)
	
	# work out which regions in the location dataframe are present in the regionlist
	allindata <- data.frame(allnames, allnames %in% datanames)
	colnames(allindata) <- c("name", "inlist")
	allpresentindata <- which(allindata$inlist == TRUE)
	
	datanames <- regionlist[datapresentinall,]
	allnames <- dataframe[allpresentindata,]

	if(length(datanames[,1]) != length(allnames[,1])) {
		cat("Something is wrong with the data provided. The number of regions found doesn't match!\n")
		return
	}
	
	# now that we have the locations that exist in both datasets, we need to remove the locations we don't have data for in the movementdata
	# first remove the non-existent origins
	origins <- as.vector(movementdata$origin)
	originsindata <- data.frame(origins, origins %in% datanames$V1)
	colnames(originsindata) <- c("id", "inlist")
	originspresentindata <- which(originsindata$inlist == TRUE)
	movementdata <- movementdata[originspresentindata,]
	
	# now do the same for the destinations
	destinations <- as.vector(movementdata$destination)
	destinationsindata <- data.frame(destinations, destinations %in% datanames$V1)
	colnames(destinationsindata) <- c("id", "inlist")
	destinationspresentindata <- which(destinationsindata$inlist == TRUE)
	movementdata <- movementdata[destinationspresentindata,]
	
	# now match up the IDs in the movementdata with those in the dataframe
	# or better yet, use the names
	# this is probably a terrible way to do it
	for(idx in 1:nrow(movementdata)) {
		rowdata <- movementdata[idx,]
		originaloriginid <- rowdata$origin
		originlocation <- (as.character(idlist[idlist$V1 == originaloriginid,2]))
		
		originaldestinationid <- rowdata$destination
		destinationlocation <- (as.character(idlist[idlist$V1 == originaldestinationid,2]))
		
		neworiginid <- (as.character(allnames[allnames$name == originlocation,2]))
		newdestinationid <- (as.character(allnames[allnames$name == destinationlocation,2]))
		
		# cat(paste("Original origin location id:", originaloriginid, "\nUpdated origin id:", neworiginid, "\nOriginal destination id:", originaldestinationid, "\nUpdated destination id:", newdestinationid, "\n"))
		
		movementdata[idx,1] <- originlocation
		movementdata[idx,2] <- destinationlocation
		movementdata[idx,3] <- as.numeric(movementdata[idx,3])
	}
	
	allnames <- allnames[order(allnames[,1]),]
	
	return (list(locations=allnames[,c(1,3,4,5)],observed=as.movementmatrix(movementdata)))	
}

showcomparisonplot <- function(optimisedmodel, observed) {
	par(mfrow=c(2,2))
	plot(raster(observed), main="Observed movement matrix")
	plot(raster(optimisedmodel$trainingresults$prediction), main="Predicted movement matrix")
	plot(raster(observed - optimisedmodel$trainingresults$prediction), main="Difference")
}

#' Kenya 2010 population raster
#'
#' An AfriPop raster of the modelled 2010 population in Kenya.
#'
#' @format A \code{RasterLayer} object in the WGS84 coordinate system at 1km
#' resolution.
#' @source This is a product of the AfriPop project
#' \url{http://www.afripop.org} provided by Professor Andy Tatem.
#'
#' @examples
#' data(kenya)
#' plot(kenya)
#'
#' @references
#' Linard C., Gilbert, M. Snow, R.W., Noor, A.M. & Tatem, A.J. (2010)
#' Population Distribution, Settlement Patterns and Accessibility across
#' Africa in 2010. PLoS ONE
#' \url{http://www.plosone.org/article/info:doi/10.1371/journal.pone.0031743}
#' @name kenya
NULL