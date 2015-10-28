###############################################################################
# Main interface methods                                                      #
###############################################################################

# set global constants: 
# usage in functions will be options()$eps or options()$ninf
.onLoad  <- function(lib, pkg){
  options(eps = sqrt(.Machine$double.eps))
  options(ninf = sqrt(.Machine$double.xmax))          
}

#' Create an optimised movement model
#'
#' Uses the \code{\link{optim}} method to create an optimised model of
#' population movements.
#' @param formula A formula with one response (a \code{movementmatrix} object) and 
#' one predictor (a \code{locationdataframe} object), e.g. movementmatrix ~ locationdataframe.
#' The \code{locationdataframe} object contains location data and the \code{movementmatrix}
#' object contains the observed population movements. 
#' @param flux_model The name of the movement model to use. Currently supported
#' models are \code{\link{originalRadiation}}, \code{\link{radiationWithSelection}},
#' \code{\link{uniformSelection}}, \code{\link{interveningOpportunities}},
#' \code{\link{gravity}} and \code{\link{gravityWithDistance}}
#' @param \dots Extra parameters to be passed to the prediction code.
#' @return An \code{optimisedmodel} object containing the training results,
#' and the optimisation results. 
#'
#' @seealso \code{\link{as.locationdataframe}}, \code{\link{is.locationdataframe}},
#' \code{\link{as.movementmatrix}}, \code{\link{is.movementmatrix}}, \code{\link{originalRadiation}}, 
#' \code{\link{radiationWithSelection}}, \code{\link{uniformSelection}}, 
#' \code{\link{interveningOpportunities}}, \code{\link{gravity}} and \code{\link{gravityWithDistance}}
#' @note The most likely format of the location data will be as a single
#' \code{data.frame} with the columns \code{location}, \code{population}, \code{lat} and 
#' \code{long}. This can be extracted from a larger dataframe with
#' \code{\link{as.locationdataframe}}
#' The \code{movement_matrix} can be extracted from a list of movements
#' using \code{\link{as.movementmatrix}}. To check that the given objects are suitable, 
#' the helper functions \code{\link{is.locationdataframe}} and \code{\link{is.movementmatrix}}
#' can be used.
#' @export
#' @examples
#' # TODO: add new exmple using the movement() function
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- raster::aggregate(kenya, 10, sum)
#' # create the prediction model for the aggregate dataset using the fixed parameter radiation model
#' #predictionModel <- movementmodel(dataset=kenya10,
#'  #                                min_network_pop = 50000,
#'  #                                flux_model = originalRadiation(),
#'  #                                symmetric = TRUE)
#' # predict the population movement from the model
#' #predictedMovements = predict(predictionModel)
#' # visualise the distance matrix
#' #sp::plot(raster::raster(predictedMovements$net$distance_matrix))
#' # visualise the predicted movements overlaid onto the original raster
#' #showprediction(predictedMovements)
movement <- function(formula, flux_model = gravity(), ...) {
  
  # receive the movementmatrix and the locationdataframe from the formula
  args  <- extractArgumentsFromFormula(formula)
  movementmatrix  <- args$movementmatrix
  locationdataframe  <- args$locationdataframe
  
  # error handling for flux_model input
  if(!is(flux_model, "flux")){
    stop("Error: Unknown flux model type given. The input 'flux_model' has to be a flux object.")
  }

  # statistics
  # http://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r
  nobs <- nrow(movementmatrix) * ncol(movementmatrix) - nrow(movementmatrix) # all values in the movementmatrix except the diagonal
  nulldf <- nobs # no predictors for null degrees of freedom
  
  # create the prediction model which is a movementmodel object
  predictionModel <- movementmodel(dataset=NULL, min_network_pop=50000, flux_model = flux_model, symmetric=FALSE)
  
  # assemble a populationdata original data.frame for predict.movementmodel to use 
  populationdata  <- data.frame(origin=locationdataframe$location, pop_origin=locationdataframe$population, long_origin=locationdataframe$x,lat_origin=locationdataframe$y)
  
  # attempt to parameterise the model using optim  
  optimresults <- attemptoptimisation(predictionModel, populationdata, movementmatrix, progress=FALSE, hessian=TRUE, ...) #, upper=upper, lower=lower
    
  # populate the training results (so we can see the end result); this is also a movementmodel object
  training_results <- predict.movementmodel(predictionModel, populationdata, progress=FALSE)
  training_results$flux_model$params <- optimresults$par
  
  cat("Training complete.\n")
  dimnames(training_results$prediction) <- dimnames(movementmatrix)
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

# read in the formula and assign to objects, with checking that the given objects are of the expected classes
extractArgumentsFromFormula <- function (formula, other = NULL) {
  
  if (length(formula) == 3) {
    
    # extracte the objects from the formula
    movementmatrix <- get(as.character(formula[[2]]))
    locationdataframe <- get(as.character(formula[[3]]))
    
    # run checks to ensure the extracted objects are of the required class
    if (is.movementmatrix(movementmatrix) &
          is.locationdataframe(locationdataframe)) {
      bad_args <- FALSE
    } else {
      bad_args <- TRUE
    }
    
  } else {
    bad_args <- TRUE
  }
  
  if (bad_args) {
    stop ('Error: formula must have one response (a movementmatrix object) and one predictor (a locationdataframe object)')
  }

  args  <- list(movementmatrix = movementmatrix, locationdataframe = locationdataframe)
  return (args)  
}

#' @title Predict from theoretical flux object
#' 
#' @description Use a \code{flux} object to predict population movements
#' given either a RasterLayer containing a single population layer, or a
#' data.frame containing population and location data with the columns
#' \code{origin} (character), \code{pop_origin} (numeric), \code{long_origin} (numeric)
#' and \code{lat_origin} (numeric).
#' 
#' The model can be calculated either for both directions (by setting the optional parameter
#' \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for
#' the summed movement between the two (\code{symmetric = TRUE}, giving a
#' symmetric matrix)).
#'
#' @param object A theoretical model of type \code{flux} object
#' @param locationdataframe A data.frame or RasterLayer containing population data
#' @param min_network_pop Optional parameter for the minimum population of a site 
#' in order for it to be processed
#' @param symmetric Optional parameter to define whether to calculate symmetric or 
#' asymmetric (summed across both directions) movement
#' @param \dots additional arguments affecting the predictions produced.
#' @return A list containing a location dataframe from the input with columns 
#' \code{location}, \code{population} and \code{coordinates} and a matrix
#' containing the predicted population movements.
#' 
#' @name predict.flux
#' @method predict flux
#' @examples
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- raster::aggregate(kenya, 10, sum)
#' # generate a flux object
#' flux <- radiationWithSelection()
#' # run the prediction for the theoretical model
#' predictedMovement  <- predict(flux, kenya10)
#' @export
predict.flux <- function(object, locationdataframe, min_network_pop = 50000, symmetric = FALSE, ...) {
  
  if(is(locationdataframe, "RasterLayer")) {
    # create the prediction model (= movementmodel object)
    predictionModel <- movementmodel(dataset = locationdataframe, min_network_pop = min_network_pop, flux_model = object, symmetric = symmetric)    
    prediction <- predict.movementmodel(predictionModel)
    df <- data.frame(location=prediction$net$locations, population=prediction$net$population, coordinates=prediction$net$coordinates)
    return (list(
      df_locations = df,
      movement_matrix = prediction$prediction))
  } else if (is(locationdataframe, "data.frame")) {
    # create the prediction model (= movementmodel object)
    predictionModel <- movementmodel(dataset=locationdataframe, min_network_pop=min_network_pop, flux_model = object, symmetric = symmetric)   
    prediction <- predict.movementmodel(predictionModel, locationdataframe)
    df <- data.frame(location=prediction$net$locations, population=prediction$net$population, coordinates=prediction$net$coordinates)
    return (list(
      df_locations = df,
      movement_matrix = prediction$prediction))
  } else {
    stop('Error: Expected parameter `locationdataframe` to be either a RasterLayer or a data.frame')
  }
}

#' Predict from an optimisedmodel object
#' 
#' \code{optimisedmodel}:
#' Use a trained \code{optimisedmodel} object to predict population movements
#' given either a RasterLayer containing a single population layer, or a
#' data.frame containing population and location data with the columns 
#' \code{origin} (character), \code{pop_origin} (numeric), \code{long_origin} (numeric)
#' and \code{lat_origin}(numeric).
#' 
#' @param object A configured prediction model of class \code{optimisedmodel}, ??
#' @param newdata An optional data.frame or RasterLayer containing population data
#' @param \dots Extra arguments to pass to the flux function
#' 
#' @return A \code{movementmodel} containing a (dense) matrix giving predicted
#' movements between all sites. \code{optimisedmodel}: A list containing a location dataframe from the input, and a matrix
#' containing the predicted population movements.
#' 
#' @name predict.optimisedmodel
#' @method predict optimisedmodel
#' @export
predict.optimisedmodel <- function(object, newdata, ...) {
  m <- object$trainingresults
  m$dataset <- newdata
  if(is(newdata, "RasterLayer")) {
    prediction <- predict.movementmodel(m)
    df <- data.frame(location=prediction$net$locations, pop=prediction$net$population, coordinates=prediction$net$coordinates)
    return (list(
      df_locations = df,
      movement_matrix = prediction$prediction))
  } else if (is(newdata, "data.frame")) {
    prediction <- predict.movementmodel(m, newdata)
    df <- data.frame(location=prediction$net$locations, pop=prediction$net$population, coordinates=prediction$net$coordinates)
    return (list(
      df_locations = df,
      movement_matrix = prediction$prediction))
  } else {
    stop('Error: Expected parameter `newdata` to be either a RasterLayer or a data.frame')
  }
}

#' @export
#' @method print optimisedmodel
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

#' @title Summarize an optimised model
#' @description Print a summary of an optimised model
#' @param object an \code{optimisedmodel} object
#' @param \dots additional arguments affecting the summary produced.
#' 
#' @name summary.optimisedmodel
#' @method summary optimisedmodel
#' @export
summary.optimisedmodel <- function(object, ...) {
  coef.p <- object$trainingresults$modelparams
  dn <- c("Estimate", "Std. Error")
  stderrors <- sqrt(abs(diag(solve(object$optimisationresults$hessian)))) # need to plug this into the coef table
  ans <- list(
    model = object$trainingresults$predictionmodel,
    deviance.resid = 1,
    coefficients = coef.p,
    nulldeviance = object$null.deviance,
    residdeviance = object$deviance,
    aic = object$aic,
    df.null = object$df.null,
    df.residual = object$df.residual,
    stderrors = stderrors)
  class(ans) <- "summary.optimisedmodel"
  return (ans)
}

#' @export
#' @method print summary.optimisedmodel
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

#' Radiation model
#'
#' The (original) radiation model generally assumes the rational of job selection. It follows the general 
#' rule that the number of employment opportunities in each district is proportional to its resident 
#' population, assuming full employment (people in district = jobs in district). Moreover, the individuals 
#' in each district choose the closest job to their home. Analytically the radiation model is represented by:
#' \deqn{T_{ij} = {\frac{PQ}{(P + R) (P + Q + R)}}}{T_ij = P * Q / (P + R) * (P + Q + R)}
#' where \eqn{P} is the population at the origin and \eqn{Q} at the destination, \eqn{R} denotes the total 
#' population in a radius \eqn{\gamma} around population centres \eqn{P_i} and \eqn{Q_j}.
#' @param theta Model parameter \code{theta} with default value and the limits theta = [0, Inf].  
#' @return A flux model object with the \code{original radiation flux} function and a set of starting parameters.
#' @references
#' Simini, F., Gonzalez, M.C., Maritan, A. & Barabasi, A.-L. (2012). A universal model for mobility and 
#' migration patterns. \emph{Nature}, 484, 96-100.
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{radiationWithSelection}}, \code{\link{uniformSelection}}, 
#' \code{\link{interveningOpportunities}}, \code{\link{gravity}}, \code{\link{gravityWithDistance}}
#' @export
originalRadiation  <- function(theta = 0.9){  
  ans  <- list(name = "original radiation", 
               params = c(theta = theta), 
               transform = c(theta = logTransform),
               flux = originalRadiationFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' Radiation with selection model
#'
#' The aim of this index is to represent travel from districts between the affected countries to other 
#' districts within the core countries. We assume that travel between districts is determined by factors 
#' such as population and distance. The radiation model with selection is defined as:
#' \deqn{ T_{ij} = \frac{\frac{1 - \lambda^{P}}{P} - \frac{1 - \lambda^{Q}}{Q}}{\frac{1 - \lambda^{R}}{R}} }{%
#' T_ij = ( 1 - \lambda^P / P ) *  ( 1 - \lambda^Q / Q ) / ( 1 - \lambda^R / R )}
#' where \eqn{P} is the population at the origin and \eqn{Q} at the destination, \eqn{R} denotes the total 
#' population in a radius \eqn{\gamma} around population centres \eqn{P_i} and \eqn{Q_j}.
#' The radiation model with selection was fitted using a set of known between district (n = 329) movements 
#' from mobile phone users from France in 2007 (Tizzoni et al. 2014). The model was then used to build a 
#' movement matrix between all districts of the core countries. District level population data were extracted 
#' using WorldPop. District level administrative boundaries were downloaded from GADM.
#' @param theta Model parameter with default value and the limits theta = [0, Inf].
#' @param lambda Model parameter with default value and the limits lambda = [0,1].
#' @return A flux model object with the \code{radiation with selection flux} function and a set of starting parameters.
#' @references
#' Simini, F., Gonzalez, M.C., Maritan, A. & Barabasi, A.-L. (2012). A universal model for mobility and 
#' migration patterns. \emph{Nature}, 484, 96-100.
#' Simini, F., Maritan, A. & Neda, Z. (2013). Human mobility in a continuum approach. \emph{PLoS One}, 8, e60069.
#' Tizzoni, M., Bajardi, P., Decuyper, A., Kon Kam King, G., Schneider, C.M., Blondel, V., et al. (2014). 
#' On the Use of Human Mobility Proxies for Modeling Epidemics. \emph{PLoS Comput. Biol.}, 10, e1003716.
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{originalRadiation}}, \code{\link{uniformSelection}}, 
#' \code{\link{interveningOpportunities}}, \code{\link{gravity}}, \code{\link{gravityWithDistance}} 
#' @export
radiationWithSelection  <- function(theta = 0.1,lambda = 0.2){  
  ans  <- list(name = "radiation with selection",
               params = c(theta = theta,lambda = lambda), 
               transform = c(theta = logTransform, lambda = unitTransform),
               flux = radiationWithSelectionFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' Uniform selection model
#'
#' The uniform selection model assumes that a job is selected uniformly at random proportionally to the 
#' population in each district following:
#' \deqn{T_{ij} = \frac{P}{Q-R}}{T_ij = P / Q - R}
#' where \eqn{P} is the population at the origin and \eqn{Q} at the destination, \eqn{R} denotes the total 
#' population in a radius \eqn{\gamma} around population centres \eqn{P_i} and \eqn{Q_j}.
#' @param theta Model parameter with default value and the limits theta = [0, Inf].
#' @return A flux model object with the \code{uniform selection flux} function and a set of starting parameters.
#' @references
#' Simini, F., Maritan, A. & Neda, Z. (2013). Human mobility in a continuum approach. \emph{PLoS One}, 8, e60069.
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{originalRadiation}}, \code{\link{radiationWithSelection}}, 
#' \code{\link{interveningOpportunities}}, \code{\link{gravity}}, \code{\link{gravityWithDistance}}
#' @export
uniformSelection  <- function(theta = 0.9){ 
  ans  <- list(name = "uniform selection",
               params = c(theta = theta), 
               transform = c(theta = logTransform),
               flux = uniformSelectionFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' Intervening opportunities
#'
#' The intervening-opportunities model (IO) assumes that the number of persons going a given distance 
#' is directly proportional to the number of opportunities at that distance and inversely proportional 
#' to the number of intervening opportunities (Stouffer 1940):
#' \deqn{T_{ij} = \frac{N_j}{d_{ij} + N_i} }{ T_ij = N_j / (d_ij + N_j) }
#' Where \eqn{N_i} is the population in location \eqn{i}, and \eqn{(d_{ij} + N_j)}{(d_ij + N_j)} is 
#' the population in all locations between \eqn{ij}. From there we apply a stochastic approach to 
#' derive a probability that a trip will terminate in location \eqn{i} is equal to the probability 
#' that \eqn{i} contains an acceptable destination and that the acceptable destination is closer to 
#' the origin \eqn{i} has not been found. Following Simini et al. 2012 the connectivity between \eqn{i}
#' and \eqn{j} becomes:
#' \deqn{T_{ij} =  e^{ -\lambda (s_{ij} + N_i)^{\alpha}} - e^{ -\lambda (s_{ij} + N_i + N_j)^{\alpha}} }{%
#' T_ij = exp(-\lambda * (s_ij + N_i)^\alpha ) - exp(-\lambda * (s_ij + N_i + N_j)^\alpha )
#' }
#' Where \eqn{e^(-\lambda)}{exp(-\lambda)} is the probability that a single opportunity is not 
#' sufficiently attractive as destination, and \eqn{\lambda} and \eqn{\alpha} are fitting parameters.
#' 
#' @param theta Model parameter with default value and the limits theta = [0, Inf].
#' @param L Model parameter with default value and the limits L = [0, Inf].
#' @return A flux model object with the \code{intervening opportunities flux} function and a set of starting 
#' parameters.
#' @references
#' Simini, F., Gonzalez, M. C., Maritan, A. & Barabasi (2012), A.-L. A universal model for mobility and migration 
#' patterns. \emph{Nature}, 484, 96-100.
#' Stouffer S. A. (1940). Intervening opportunities: a theory relating mobility and distance. \emph{Am. 
#' Sociol. Rev.} 5, 845-867.
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{originalRadiation}}, \code{\link{radiationWithSelection}}, 
#' \code{\link{uniformSelection}}, \code{\link{gravity}}, \code{\link{gravityWithDistance}} 
#' @export
interveningOpportunities  <- function(theta = 0.001, L = 0.00001){    
  params = c(theta = theta, L = L)
  ans  <- list(name = "intervening opportunities", 
               params = params, 
               transform = c(theta = logTransform, L = logTransform),
               flux = interveningOpportunitiesFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' Gravity model
#'
#' The gravity law assumes that the number of people moving between locations is 
#' proportional to some power of the origin and destination population, and decays 
#' by distance between them following: 
#'\deqn{T_{ij} = \frac{m_i^\alpha \times n_j^\beta }{f(r_{ij})}}{T_ij = m_i^\alpha * n_j^\beta / f(r_ij)}
#' where, \eqn{m_i} represents the population at origin, \eqn{n_j} the population at the destination 
#' and \eqn{r_{ij}}{r_ij} the distance between them. \eqn{\alpha} and \eqn{\beta} are tuning parameters 
#' fitted to each subpopulation size, and \eqn{f(r_{ij})}{f(r_ij)} is a distance-dependent functional 
#' form.
#' @param theta Model parameter with default value and the limits theta = [0, Inf].
#' @param alpha Model parameter with default value and the limits alpha = [-Inf, Inf].
#' @param beta Model parameter with default value and the limits alpha = [-Inf, Inf].
#' @param gamma Model parameter with default value and the limits gamma = [-Inf, Inf].
#' @return A flux model object with the \code{gravity flux} function and a set of starting parameters.
#' @references
#' Zipf, G.K. (1946). The P1 P2 / D hypothesis: on the intercity movement of persons. \emph{Am. Sociol. Rev.}, 
#' 11, 677-686.
#' Balcan, D., Colizza, V., Gonc, B. & Hu, H. (2009). Multiscale mobility networks and the spatial. 
#' \emph{Proc. Natl. Acad. Sci. U. S. A.}, 106, 21484-9. 
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{originalRadiation}}, \code{\link{radiationWithSelection}}, 
#' \code{\link{uniformSelection}}, \code{\link{interveningOpportunities}}, \code{\link{gravityWithDistance}}
#' @export
gravity  <- function(theta = 0.01, alpha = 0.06, beta = 0.03, gamma = 0.01){  
  ans  <- list(name = "gravity", 
               params = c(theta = theta, alpha = alpha, beta = beta, gamma = gamma), 
               transform = c(theta = logTransform, alpha = identityTransform, beta = identityTransform, 
                             gamma = identityTransform),
               flux = gravityFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' Gravity with distance model
#' 
#' In order to obtain more accurate results, following Viboud et al. 2006 we implement a nine-parameter 
#' form of the gravity law, in which short and long trips are fitted separately. Similarly to the gravity 
#' model we fit each parameter (equation 1) using a Poisson regression:
#' \deqn{T_{ij} = \theta \frac{ N_i^{\alpha} N_j^{\beta} }{d_{ij}^{\gamma}} }{%
#' T_ij = \theta * N_i^\alpha * N_j^{\beta} / d_ij^{\gamma} }
#' where \eqn{\theta} is a proportionality constant and the exponents \eqn{\alpha} and \eqn{\beta} respectively, 
#' tune the dependence of dispersal on donor and recipient population sizes (\eqn{N}), and the distance between 
#' the two communities \eqn{d_{ij}^{\gamma}}{d_ij^\gamma}. By taking the logarithm of on both sides this becomes: 
#' \deqn{\ln(T_{ij}) = \ln(\theta) + \alpha \ln(N_i) + \beta \ln{N_j} - \gamma \ln(d_{ij}) }{%
#'  ln(T_ij) = ln(\theta) + \alpha ln(N_i) + \beta ln{N_j} - \gamma ln(d_ij)
#'  }
#' Viboud et al. show that below 119km, the population exponents are relatively high and larger for the 
#' destination population. Therefore we allow the flexibility to adjust based on a distance cutoff for the model.
#' @param theta1 Model parameter with default value and the limits theta = [0, Inf].
#' @param alpha1 Model parameter with default value and the limits alpha = [-Inf, Inf].
#' @param beta1 Model parameter with default value and the limits beta = [-Inf, Inf].
#' @param gamma1 Model parameter with default value and the limits gamma = [-Inf, Inf].
#' @param delta Model parameter with default value and the limits delta = [0, 1].
#' @param theta2 Model parameter with default value and the limits theta = [0, Inf].
#' @param alpha2 Model parameter with default value and the limits alpha = [-Inf, Inf].
#' @param beta2 Model parameter with default value and the limits beta = [-Inf, Inf].
#' @param gamma2 Model parameter with default value and the limits gamma = [-Inf, Inf].
#' @return A flux model object with the \code{gravity with distance flux} function and a set of starting 
#' parameters.
#' @references
#' Viboud, C. et al. (2006). Synchrony, waves, and spatial hierarchies in the spread of influenza. \emph{Science}, 
#' 312, 447-51
#' @note Limits \eqn{0} and \eqn{Inf} will be changed internally to the numerically safe approximations
#' \eqn{0 -> sqrt(.Machine$double.eps)} and \eqn{Inf -> sqrt(.Machine$double.xmax)}, respectively.
#' @seealso \code{\link{movement}}, \code{\link{originalRadiation}}, \code{\link{radiationWithSelection}}, 
#' \code{\link{uniformSelection}}, \code{\link{interveningOpportunities}}, \code{\link{gravity}}
#' @export
gravityWithDistance  <- function(theta1 = 0.01, alpha1 = 0.06, beta1 = 0.03, gamma1 = 0.01, 
                                 delta = 0.5, theta2 = 0.01, alpha2 = 0.06, beta2 = 0.03, 
                                 gamma2 = 0.01){  
  ans  <- list(name = "gravity with distance", 
               params = c(theta1 = theta1, alpha1 = alpha1, beta1 = beta1, gamma1 = gamma1, 
                          delta = delta, theta2 = theta2, alpha2 = alpha2, beta2 = beta2, gamma2 = gamma2), 
               transform = c(theta1 = logTransform, alpha1 = identityTransform, beta1 = identityTransform, 
                             gamma1 = identityTransform, delta = unitTransform, theta2 = logTransform, 
                             alpha2 = identityTransform, beta2 = identityTransform, gamma2 = identityTransform),
               flux = gravityWithDistanceFlux)
  class(ans)  <- 'flux'
  return(ans)
}

#' @title Print details of a flux object 
#' @description Print details of a given flux object
#' @param x a \code{flux} object
#' @param \dots further arguments to be passed to or from other methods. 
#' They are ignored in this function. 
#' @name print.flux
#' @method print flux
#' 
#' @examples
#' flux <- gravity(theta = 0.1, alpha = 0.5, beta = 0.1, gamma = 0.1)
#' print(flux)
#' @export
print.flux  <- function(x, ...){
  cat(paste('flux object for a ', x$name, 'model with parameters\n\n'))
  print.default(format(x$params),
                print.gap = 2, quote = FALSE)
  cat('\n')
  cat('See ?')
  cat(paste(x$name, 'for the model formulation and explanation of parameters\n'))
}

#' @title Print summary of a flux object 
#' @description Print summary of a given flux object
#' @param object a \code{flux} object
#' @param \dots additional arguments affecting the summary produced.
#' @name summary.flux
#' @method summary flux
#'
#' @examples
#' flux <- gravity(theta = 0.1, alpha = 0.5, beta = 0.1, gamma = 0.1)
#' summary(flux)
#' @export
summary.flux  <- function(object, ...){
  print(object)
}

# Use the original radiation model of Simini et al. (2013) to predict movement between
# two sites based on population and distance.
#
# Given indices \code{i} and \code{j}, a (dense) distance matrix
# \code{distance} giving the euclidean distances between all pairs of sites, a
# vector of population sizes \code{population} and a set of parameters, the 
# original radiation model as variant of the continuum model (Simini et al. 2013) 
# is used to predict movements between sites \code{i} and \code{j}. The mathematical definition
# of the model and an explanation of how further continuum models are 
# related to each other can be found in Simini et al. (2013).
# The parameter, which is required, is supplied as the first element of
# the vector \code{theta}. This parameter describes the proportion of all
# inhabitants in the region commuting. The default is that everyone commutes
# and thus \code{theta[1]=1}. 
# The flux can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, returning movements for each direction) or for the
# summed movement between the two (\code{symmetric = TRUE}).
# The model can be speed up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual site pairs. To calculate
# movements across a whole landscape, use \code{\link{movement.predict}}.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta A vector of the parameter which reflects the proportion of all
# inhabitants in the region commuting
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second element)
# @param minpop The minimum population size to consider (by default 1,
# consider all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the original radiation model
# T_ij <- originalRadiationFlux(3, 4, d, pop)
# T_ij
#
# @seealso \code{\link{movement.predict}}, \code{\link{radiationWithSelectionFlux}},
# \code{\link{uniformSelectionFlux}}, \code{\link{interveningOpportunitiesFlux}}
#
# @references
# Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach.
# \emph{PLoS ONE} 8(3): e60069.
# \url{http://dx.doi.org/10.1371/journal.pone.0060069}
originalRadiationFlux <- function(i, j, distance, population,
                           theta = c(1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {
  # get model parameters
  p <- theta[1]
    
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
  
  P_nam_ij <- m_i_times_n_j / ((m_i + i_s_ij) * (m_i_plus_n_j + i_s_ij))
  P_nam_ji <- m_i_times_n_j / ((n_j + j_s_ij) * (m_i_plus_n_j + j_s_ij))
  
  T_ij <- T_i * P_nam_ij
  
  # and in the opposite direction
  T_ji <- T_j * P_nam_ji
    
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

# Use the radiation with selection model of Simini et al. (2013) to predict 
# movement between two sites based on population and distance.
#
# Given indices \code{i} and \code{j}, a (dense) distance matrix
# \code{distance} giving the euclidean distances between all pairs of sites, a
# vector of population sizes \code{population} and a set of parameters, the
# radiation with selection model as variant of the continuum model (Simini et al. 2013) 
# is used to predict movements between sites \code{i} and \code{j}.
# The mathematical definition of the model and and an explanation of how further 
# continuum models are related to each other can be found in Simini et al. (2013).
# The first parameter, which is required, is supplied as the first element of
# the vector \code{theta}. This parameter describes the proportion of all
# inhabitants in the region commuting. The default is that everyone commutes
# and thus \code{theta[1]=1}. The second (and last) element of \code{theta}
# supplies a parameter that is also necessary for the radiation with selection 
# variants of the model.
# The flux can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, returning movements for each direction) or for the
# summed movement between the two (\code{symmetric = TRUE}).
# The model can be sped up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual site pairs. To calculate
# movements across a whole landscape, use \code{\link{movement.predict}}.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta A vector of parameters in the order: proportion of all
# inhabitants in the region commuting, and a second parameter required for the  
# radiation with selection model variants.
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second element)
# @param minpop The minimum population size to consider (by default 1,
# consider all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the original radiation model
# T_ij <- radiationWithSelectionFlux(3, 4, d, pop, c(0.1,0.1))
# T_ij
#
# @seealso \code{\link{movement.predict}}, \code{\link{originalRadiationFlux}},
# \code{\link{uniformSelectionFlux}}, \code{\link{interveningOpportunitiesFlux}}
#
# @references
# Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach.
# \emph{PLoS ONE} 8(3): e60069.
# \url{http://dx.doi.org/10.1371/journal.pone.0060069}
radiationWithSelectionFlux <- function(i, j, distance, population,
                           theta = c(1,1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {
  # get model parameters
  p <- theta[1]
  lambda <- theta[2]
    
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
  
  P_nam_ij <-
    ((1 - lambda ^ (m_i + i_s_ij + 1)) / (m_i + i_s_ij + 1) -
       (1 - lambda ^ (m_i_plus_n_j + i_s_ij + 1)) / (m_i_plus_n_j + i_s_ij + 1)) / ((1 - lambda ^ (m_i + 1)) / (m_i + 1))
  P_nam_ji <-
    ((1 - lambda ^ (n_j + j_s_ij + 1)) / (n_j + j_s_ij + 1) -
       (1 - lambda ^ (m_i_plus_n_j + j_s_ij + 1)) / (m_i_plus_n_j + j_s_ij + 1)) / ((1 - lambda ^ (n_j + 1)) / (n_j + 1))
  
  T_ij <- T_i * P_nam_ij
  
  # and in the opposite direction
  T_ji <- T_j * P_nam_ji
  
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}


# Use the uniform selection model of Simini et al. (2013) to predict movement between
# two sites based on population and distance.
#
# Given indices \code{i} and \code{j}, a (dense) distance matrix
# \code{distance} giving the euclidean distances between all pairs of sites, a
# vector of population sizes \code{population} and a set of parameters, the 
# uniform selection model as variant of the continuum model (Simini et al. 2013)
# is used to predict movements between sites \code{i} and \code{j}.
# The mathematical definition of the model and an explanation of how further 
# continuum models are related to each other can be found in Simini et al. (2013).
# The parameter, which is required, is supplied as the first element of
# the vector \code{theta}. This parameter describes the proportion of all
# inhabitants in the region commuting. The default is that everyone commutes
# and thus \code{theta[1]=1}.
# The flux can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, returning movements for each direction) or for the
# summed movement between the two (\code{symmetric = TRUE}).
# The model can be sped up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual site pairs. To calculate
# movements across a whole landscape, use \code{\link{movement.predict}}.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta  A vector of the parameter which reflects the proportion of all
# inhabitants in the region commuting
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second element)
# @param minpop The minimum population size to consider (by default 1,
# consider all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the original radiation model
# T_ij <- uniformSelectionFlux(3,4,d,pop,theta = c(0.9))
# T_ij
#
# @seealso \code{\link{movement.predict}}, \code{\link{originalRadiationFlux}}
# \code{\link{radiationWithSelectionFlux}}, \code{\link{interveningOpportunitiesFlux}}
#
# @references
# Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach.
# \emph{PLoS ONE} 8(3): e60069.
# \url{http://dx.doi.org/10.1371/journal.pone.0060069}
uniformSelectionFlux <- function(i, j, distance, population,
                           theta = c(1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {
  # get model parameters
  p <- theta[1]
    
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
  
  N <- sum(population)
  P_nam_ij <- n_j / (N - m_i)
  P_nam_ji <- m_i / (N - n_j)
  
  T_ij <- T_i * P_nam_ij
  
  # and in the opposite direction
  T_ji <- T_j * P_nam_ji
  
  
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

# Use the intervening opportunities model of Simini et al. (2013) to predict movement between
# two sites based on population and distance.
#
# Given indices \code{i} and \code{j}, a (dense) distance matrix
# \code{distance} giving the euclidean distances between all pairs of sites, a
# vector of population sizes \code{population} and a set of parameters, the 
# intervening opportunities model as variant of the continuum model (Simini et al. 2013) 
# is used to predict movements between sites \code{i} and \code{j}.
# The mathematical definition of the model and an explanation of how further 
# continuum models are related to each other can be found in Simini et al. (2013).
# The first parameter, which is required, is supplied as the first element of
# the vector \code{theta}. This parameter describes the proportion of all
# inhabitants in the region commuting. The default is that everyone commutes
# and thus \code{theta[1]=1}. The second (and last) element of \code{theta}
# supplies a parameter that is also necessary for the intervening opportunities
# variants of the model.
# The flux can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, returning movements for each direction) or for the
# summed movement between the two (\code{symmetric = TRUE}).
# The model can be sped up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual site pairs. To calculate
# movements across a whole landscape, use \code{\link{movement.predict}}.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta A vector of parameters in the order: proportion of all
# inhabitants in the region commuting, parameter required for the
# intervening opportunities model variants.
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second
# element)
# @param minpop The minimum population size to consider (by default 1,
# consider all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the original radiation model
# T_ij <- interveningOpportunitiesFlux(3, 4, d, pop, theta = c(0.1, 0.1))
# T_ij
#
# @seealso \code{\link{movement.predict}}, \code{\link{originalRadiationFlux}},  
# \code{link{radiationWithSelectionFlux}}, \code{\link{uniformSelectionFlux}}
#
# @references
# Simini F, Maritan A, Neda Z (2013) Human mobility in a continuum approach.
# \emph{PLoS ONE} 8(3): e60069. \url{http://dx.doi.org/10.1371/journal.pone.0060069}
interveningOpportunitiesFlux <- function(i, j, distance, population,
                           theta = c(1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {
  # get model parameters
  p <- theta[1]
  L <- theta[2]
  
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
    
  P_nam_ij <- (exp(-L * (m_i + i_s_ij)) - exp(-L * (m_i_plus_n_j + i_s_ij))) / exp(-L * m_i)
  P_nam_ji <- (exp(-L * (n_j + j_s_ij)) - exp(-L * (m_i_plus_n_j + j_s_ij))) / exp(-L * n_j)
  
  T_ij <- T_i * P_nam_ij
  
  # and in the opposite direction
  T_ji <- T_j * P_nam_ji
    
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

# Use the Viboud et al. 2006 (relatively simple) gravitation model to predict
# movement between two sites.
#
# Given indices \code{i} and \code{j}, a vector of population sizes
# \code{population}, a (dense) distance matrix \code{distance} giving the
# euclidean distances between all pairs of sites, and a set of parameters
# \code{theta}, to predict movements between sites \code{i} and \code{j}.
# The flux can be calculated either for both directions (by setting
#  \code{symmetric = FALSE}, returning movements for each direction) or for
#  the summed movement between the two (\code{symmetric = TRUE}).
# The model can be sped up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual sites, use
# \code{\link{movement.predict}} to calculate movements for multiple
# populations.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta A vector of parameters in the order: scalar, exponent on donor
# pop, exponent on recipient pop, exponent on distance
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second element)
# @param minpop The minimum population size to consider (by default 1, consider
# all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the radiation model
# T_ij <- gravityFlux(3, 4, d, pop, theta=c(1e-4,0.6,0.3,3))
# T_ij
#
# @seealso \code{\link{movement.predict}}
#
# @references
# Viboud et al. (2006) Synchrony, Waves, and Spatial Hierarchies in the Spread
# of Influenza. \emph{Science} \url{http://dx.doi.org/10.1126/science.1125237}
gravityFlux <- function(i, j, distance, population,
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

# Use the Simini et al. 2012 modified gravitation model to predict
# movement between two sites.
#
# Given indices \code{i} and \code{j}, a vector of population sizes
# \code{population}, a (dense) distance matrix \code{distance} giving the
# euclidean distances between all pairs of sites, and a set of parameters
# \code{theta}, to predict movements between sites \code{i} and \code{j}.
# The flux can be calculated either for both directions (by setting
#  \code{symmetric = FALSE}, returning movements for each direction) or for
#  the summed movement between the two (\code{symmetric = TRUE}).
# The model can be sped up somewhat by setting \code{minpop} and
# \code{maxrange}. If either of the two sites has a population lower than
# \code{minpop} (minimum population size), or if the distance between the two
# sites is greater than \code{maxrange} (the maximum range) it is assumed that
# no travel occurs between these points.
# Note that this function only works for individual sites, use
# \code{\link{movement.predict}} to calculate movements for multiple
# populations. The modification from Simini et al. introduces a nine parameter
# form where a different set of parameters are used if the distance is greater
# than \code{delta}.
#
# @param i Index for \code{population} and \code{distance} giving the first site
# @param j Index for \code{population} and \code{distance} giving the second site
# @param distance A distance matrix giving the euclidean distance between pairs of sites
# @param population A vector giving the population at all sites
# @param theta A vector of nine parameters in the order: scalar1,
#  exponent1 on donor pop, exponent1 on recipient pop, exponent1 on distance,
#  theshhold, scalar2, exponent2 on donor pop, exponent2 on recipient pop,
#  exponent2 on distance. The first four parameters are used as the parameters
#  of \code{gravityFlux} if the distance is less than threshold (5th
#  parameter), otherwise the last four parameters are used for the gravity
#  flux.
# @param symmetric Whether to return a single value giving the total predicted
# movements from i to j and j to i (if \code{TRUE}) or vector of length 2
# giving movements from i to j (first element) and from j to i (second element)
# @param minpop The minimum population size to consider (by default 1, consider
# all sites)
# @param maxrange The maximum distance between sites to consider (by default
# \code{Inf}, consider all sites)
# @return A vector (of length either 1 or 2) giving the predicted number of
# people moving between the two sites.
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict movement between sites 3 and 4 using the radiation model
# T_ij <- gravityWithDistanceFlux(3, 4, d, pop, theta=c(1e-4,0.6,0.3,3,1,1e-4,0.6,0.3,3))
# T_ij
#
# @references
# Viboud et al. (2006) Synchrony, Waves, and Spatial Hierarchies in the Spread
# of Influenza. \emph{Science} \url{http://dx.doi.org/10.1126/science.1125237}
gravityWithDistanceFlux <- function(i, j, distance, population,
                                     theta = c(1, 0.6, 0.3, 3, 1, 1, 0.6, 0.3, 3),
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
  
  mytheta = theta[1:4]
  
  if(r_ij > theta[5]) {
    mytheta = theta[6:9]
  }
  
  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$ using equation 1 in Viboud et al. (2006)
  T_ij <- mytheta[1] * (m_i ^ mytheta[2]) * (n_j ^ mytheta[3]) / (r_ij ^ mytheta[4])
  
  # and the opposite direction
  T_ji <- mytheta[1] * (n_j ^ mytheta[2]) * (m_i ^ mytheta[3]) / (r_ij ^ mytheta[4])
  
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}

# Use a movement model to predict movements across a landscape.
#
# Given a (dense) distance matrix \code{distance} giving the euclidean
# distances between all pairs of sites, a vector of population sizes at these
# sites \code{population}, use a flux function \code{flux} to predict movement
# between all sites.
# The model can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for
# the summed movement between the two (\code{symmetric = TRUE}, giving a
# symmetric matrix)). Progress and start and end times can be displayed by
# setting \code{progress = TRUE} and arguments of the flux functions can
# specified using the \code{dots} argument.
#
# @param distance A distance matrix giving the euclidean distance between
# pairs of sites
# @param population A vector giving the population at all sites
# @param flux A flux function (currently either \code{original radiation flux},
# \code{radiation with selection flux}, \code{uniform selection flux},
# \code{intervening opportunities flux}, \code{gravity flux}
# or \code{gravity with distance flux}) used to predict movements
# @param symmetric Whether to calculate symmetric or asymmetric (summed
# across both directions) movement
# @param progress Whether to display a progress bar and start and end times
# - can be useful for big model runs
# @param \dots Arguments to pass to the flux function
# @return A (dense) matrix giving predicted movements between all sites
#
# @examples
# # generate random coordinates and populations
# n <- 30
# coords <- matrix(runif(n * 2), ncol = 2)
# pop <- round(runif(n) * 1000)
# # calculate the distance between pairs of sites
# d <- as.matrix(dist(coords))
# # predict total movement between them using the radiation model
# move <- movement.predict(d, pop, flux = originalRadiationFlux, symmetric = TRUE,
#     theta = 0.1)
# # plot the points
# plot(coords, pch = 16, cex = pop / 500,
#      col = 'grey40', axes = FALSE,
#      xlab = '', ylab = '')
# # and add arrows showing movement
# for(i in 2:n) {
#   for(j in  (i - 1):n) {
#     arrows(coords[i, 1],
#            coords[i, 2],
#            coords[j, 1],
#            coords[j, 2],
#            lwd = 2,
#            length = 0,
#            col = rgb(0, 0, 1, move[i, j] / (max(move) + 1)))
#   }
# }
movement.predict <- function(distance, population,
                             flux = originalRadiationFlux,
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

#' Display the movement predictions on a plot
#'
#' @param network A list containing a population vector, distance matrix and
#' sets of coordinates for each location
#' @param raster_layer A base layer to plot the predictions onto.
#' @param predictedMovements A data.frame containing predicted movements
#' between locations.
#' @param \dots Extra parameters to pass to plot
#'
#' @export
show.prediction <- function(network, raster_layer, predictedMovements, ...) {
  # visualise the distance matrix
  sp::plot(raster::raster(network$distance_matrix))
  
  # plot the raster layer
  sp::plot(raster_layer, ...)
  
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

#' @title Plot movement predictions
#'
#' @name showprediction
#'
#' @description Given a movement model, plot the underlying
#' raster, the configured location points and the predicted movements
#' between locations.
#'
#' @param object A configured prediction model
#' @param \dots Extra parameters to pass to plot
#'
#' @export
showprediction <- function(object, ...) {
  UseMethod("showprediction", object)
}

#' @rdname showprediction
#'
#' @export 
#' @method showprediction default
showprediction.default <- function(object, ...) {
  print("showprediction doesn't know how to handle this object.")
  return (object)
}

# @rdname showprediction
# 
# @method showprediction movementmodel
showprediction.movementmodel <- function(object, ...) {
  network <- object$net
  move <- object$prediction
  raster <- object$dataset
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
#'  \item{distance_matrix }{A distance matrix (either of class \code{matrix} or
#' \code{dist}) diving the pairwise euclidean distance between the cells of
#' interest in the units of \code{raster}}
#'  \item{coordinate }{A two-column matrix giving the coordinates of the cells
#' of interest in the units of \code{raster}}
#'
#' @examples
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- raster::aggregate(kenya, 10, sum)
#' # get the network for pixels with at least 50,000 inhabitants
#' net <- get.network(kenya10, min = 50000)
#' # visualise the distance matrix
#' sp::plot(raster::raster(net$distance_matrix))
#' # plot the raster layer
#' sp::plot(kenya10)
#' # rescale the population of those pixels for plotting
#' size <- 0.1 + 2 * net$population / max(net$population)
#' # plot the pixels selected, with point size proportional to population size
#' points(net$coordinates, pch = 16,
#'      cex = size,
#'      col = rgb(0, 0, 1, 0.6))
#'
#' @seealso \code{\link{raster}}, \code{\link{dist}}
#' @export
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
  coords <- raster::xyFromCell(raster, keep)
  
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

# Extract the necessary components for movement modelling form a population
# movement data.frame.
#
# Given population movement data.frame, extract a distance matrix and vector
# of population sizes for all cells with population density above a minimum
# threshold. These can be used as network representation of the landscape for
# use in movement models.
#
# @param dataframe A data.frame object of containing population, and
# location data.
# @param min The minimum population size for inclusion in the network. All
# cells with populations greater than or equal to \code{min} will be included
# and other excluded.
# @param matrix Whether the distance matrix should be returned as a
# \code{matrix} object (if \code{TRUE}) or as a \code{dist} object (if
# \code{FALSE}).
# @return A list with three components:
#  \item{population }{A vector giving the populations at the cells of
# interest}
#  \item{distance_matrix }{A distance matrix (either of class \code{matrix} or
# \code{dist}) diving the pairwise euclidean distance between the cells of
# interest in the units of \code{raster}}
#  \item{coordinate }{A two-column matrix giving the coordinates of the cells
# of interest in the units of \code{raster}}
getNetworkFromdataframe <- function(dataframe, min = 1, matrix = TRUE) {
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

# Create a movement model to predict movements across a landscape.
#
# This sets up a movement model object which can then be used to predict
# population movements. It requires a \code{raster} dataset and a number of
# optional parameters to set up the basic configuration of the prediction
# model.
# The model can be calculated either for both directions (by setting
# \code{symmetric = FALSE}, resulting in an asymmetric movement matrix) or for
# the summed movement between the two (\code{symmetric = TRUE}, giving a
# symmetric matrix)).
#
# @param dataset A raster dataset of population data
# @param min_network_pop The minimum population of a site in order for it to be
# processed
# @param flux_model A flux object used to calculated
# predicted flux between locations. Currently supported prediction models are
# \code{gravity}, \code{original radiation}, \code{intervening opportunities},
# \code{radiation with selection} and \code{uniform selection}
# @param symmetric Whether to calculate symmetric or asymmetric (summed across
# both directions) movement
# @return A movement model object which can be used to run flux predictions.
movementmodel <- function(dataset, min_network_pop = 50000, flux_model = originalRadiation(), symmetric = TRUE) {
  me <- list(
    dataset = dataset,
    min_network_pop = min_network_pop,
    flux_model = flux_model,
    symmetric = symmetric
  )
  class(me) <- "movementmodel"
  return (me)
}

# Predictions from movementmodel objects
# 
# Given a movement model, use the configured distances and
# flux function to predict movement between all sites.
# Any extra arguments of the flux functions can specified using the
# \code{dots} argument.
# 
# @param object A configured prediction model of class \code{movementmodel}
# @param newdata An optional data.frame or RasterLayer containing population data
# @param \dots Extra arguments to pass to the flux function
# @return A \code{movementmodel} containing a (dense) matrix giving predicted
# movements between all sites. \code{optimisedmodel}: A list containing a location dataframe from the input, and a matrix
# containing the predicted population movements.
# 
# @name predict.movementmodel 
# @method predict movementmodel
predict.movementmodel <- function(object, newdata = NULL, ...) {
  if(is.null(newdata)) {
    net <- get.network(object$dataset, min = object$min_network_pop)
  }
  else {
    net <- getNetworkFromdataframe(newdata, min = object$min_network_pop)
  }
  object$net = net
  
  object$prediction = movement.predict(distance = net$distance_matrix, population = net$population, flux = object$flux_model$flux, 
                                                symmetric = object$symmetric, theta = object$flux_model$params, ...)    
   
  return (object)
}

# Calculate the log likelihood of the prediction given the observed data.
#
# Processes an observed and predicted matrix, strips out the diagonals (which
# should be zero) and calculates the log likelihood.
#
# @param prediction A square matrix containing the predicted movements between location IDs
# @param observed A square matrix containing the observed movements between location IDs
# @return The log likelihood
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

# Internal helper function for optimisation
#
# Calls the model prediction code and then calculates a log likelihood metric
# used as the \code{\link{optim}} minimisation value
#
# @param par theta values for the flux function
# @param predictionModel The prediction model of movementmodel object type being optimised 
# @param observedmatrix A matrix containing the observed population movements
# @param populationdata A dataframe containing population coordinate data
# @param \dots Parameters passed to \code{\link{ict}}
# @return The log likelihood of the prediction given the observed data.
fittingwrapper <- function(par, predictionModel, observedmatrix, populationdata, ...) {
  # the flux function requires the untransformed (i.e. original, constraint) parameters and therefore, need to perform 
  # the inverse transformation here 
  originalParams  <- transformFluxObjectParameters(par, predictionModel$flux_model$transform, inverse = TRUE)
  predictionModel$flux_model$params <- originalParams
  predictedResults <- predict.movementmodel(predictionModel, populationdata, ...)
  loglikelihood <- analysepredictionusingdpois(predictedResults, observedmatrix)
  return (loglikelihood)
}

# Attempt to optimise the parameters of a given movement model based on log
# likelihoods against observed data.
#
# Runs the optim function using the BFGS optimisation method to try and
# optimise the parameters of the given prediction model.
#
# @param predictionModel A configured prediction model
# @param populationdata A dataframe containing population data linked to the
# IDs in the predictionModel raster. Requires 4 columns named \code{origin},
# \code{pop_origin}, \code{long_origin}, \code{lat_origin}
# @param observedmatrix A matrix containing observed population movements. Row
# and column numbers correspond to the indexes of a sorted list of the origins
# and destinations used in populationdata. Values are the actual population
# movements.
# @param \dots Parameters passed to \code{movement.predict}
# @return See \code{\link{optim}}
#
# @seealso \code{\link{createobservedmatrixfromcsv}}
attemptoptimisation <- function(predictionModel, populationdata, observedmatrix, ...) {

  # transform the flux object parameters to unconstraint values using the helper function
  transformedParams  <- transformFluxObjectParameters(predictionModel$flux_model$params,predictionModel$flux_model$transform, FALSE)
  
  # run optimisation on the prediction model using the BFGS method. The initial parameters set in the prediction model are used as the initial par value for optimisation
  # the optim() function require the transformed (i.e. = unconstraint) parameters to be optimized over
  optimresults  <- optim(transformedParams, fittingwrapper, method="BFGS", predictionModel = predictionModel, observedmatrix = observedmatrix, populationdata = populationdata, ...)

  # perform the inverse transformation on the optimised parameters into its true (i.e. constraint) scale
  optimresults$par  <- transformFluxObjectParameters(optimresults$par, predictionModel$flux_model$transform, TRUE)
  
  return (optimresults)
}

###############################################################################
# Data manipulation helper functions                                          #
###############################################################################

#' Convert a shapefile to a \code{RasterLayer}
#'
#' Converts a given shapefile and converts it to a \code{RasterLayer} and
#' discards any unnecessary layers.
#'
#' @param filename The path to the shapefile to convert
#' @param keeplist A list of layers in the shapefile to keep
#' @param n The multiplier to use when creating the raster from the extent
#' @return A \code{RasterLayer} object
#' @export
rasterizeShapeFile <- function(filename, keeplist,n=5)  {
  # load the shapefile into a SpatialPolygonsDataFrame
  dsn = dirname(filename)
  filename = basename(filename)
  layer = tools::file_path_sans_ext(filename)
  shapeObject = rgdal::readOGR(dsn = dsn, layer = layer)
  shapeObject <- shapeObject[keeplist]
  
  # get the extents of the dataframe
  extents = raster::extent(shapeObject)
  xmin = extents@xmin
  xmax = extents@xmax
  ymin = extents@ymin
  ymax = extents@ymax
  
  # set up a raster template to use in rasterize()
  ext <- raster::extent (xmin, xmax, ymin, ymax)
  xy <- abs(apply(as.matrix(sp::bbox(ext)), 1, diff))
  r <- raster::raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)
  
  rr <- raster::rasterize(shapeObject, r)
  ## create a population only rasterlayer (i.e. remove the RAT table)
  rr <- raster::deratify(rr, keeplist)
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
#' @export
createobservedmatrixfromcsv <- function(filename, origincolname, destcolname, valcolname) {
  data <- read.csv(file=filename,header=TRUE,sep=",")
  nrows = length(unique(data[origincolname])[,1])
  ncols = length(unique(data[destcolname])[,1])
  
  # mapping locationids to matrix row or column ids to cope with missing locationids
  origins = sort(as.numeric(unique(data[origincolname])[,1]))
  destinations = sort(as.numeric(unique(data[destcolname])[,1]))
  
  sparseMatrix <- matrix(nrow = nrows, ncol = ncols)
  for (idx in 1:length(data[,1])) {
    sparseMatrix[match(data[idx,origincolname],origins),match(data[idx,destcolname],destinations)] = data[idx,valcolname]
  }
  
  # set any remaining NAs to 0
  sparseMatrix[is.na(sparseMatrix)] <- 0
  
  return (sparseMatrix)
}

#' @title Conversion to locationdataframe
#' 
#' @description Convert objects to \code{locationdataframe} objects
#' 
#' @param dataframe object to convert to a \code{locationdataframe} object.
#' Either a data.frame with columns \code{origin} (character), \code{destination} (character), \code{movement} (numeric),
#' \code{pop_origin} (numeric), \code{pop_destination} (numeric), \code{lat_origin} (numeric), \code{long_origin} (numeric),
#' \code{lat_destination} (numeric) and \code{long_destination} (numeric) or a \code{SpatialPolygonsDataFrame} object
#' 
#' @param \dots further arguments passed to or from other methods.
#' 
#' @return A data.frame containing location data with columns \code{location} (character), \code{pop} (numeric), 
#' \code{lat} (numeric) and \code{lon} (numeric).
#' @name as.movementmatrix
#' @export
as.movementmatrix <- function(dataframe) {
  nrows <- length(unique(dataframe[1])[,])
  ncols <- length(unique(dataframe[2])[,])
  if(nrows != ncols) {
    stop ("Error: Expected a square matrix!")
  }
  
  mat <- matrix(ncol = ncols, nrow = nrows, dimnames = list(unique(dataframe[1])[,],unique(dataframe[2])[,]))
  for(idx in 1:nrow(dataframe)) {
    mat[as.character(dataframe[idx,2]),as.character(dataframe[idx,1])] <- dataframe[idx,3]
  }
  
  mat[is.na(mat)] <- 0	
  
  mat <- mat[order(rownames(mat)),]
  mat <- mat[,order(colnames(mat))]
  
  class(mat) <- c('matrix', 'movementmatrix')
  return (mat)
}

#' @title  Check if given matrix if 'movementmatrix' object
#'
#' @description
#' Helper function checking that the given object inherits from \code{movementmatrix} class. 
#' 
#' @param x The object to be checked.
#' @return True if the given object is a \code{movementmatrix} object; false otherwise
#' @export
is.movementmatrix <- function(x) {
  res  <- inherits(x, 'movementmatrix')
  return(res)
}


#' @title Conversion to locationdataframe
#' 
#' @description Convert objects to \code{locationdataframe} objects
#' 
#' @param input object to convert to a \code{locationdataframe} object.
#' Either a data.frame with columns \code{origin} (character), \code{destination} (character), \code{movement} (numeric),
#' \code{pop_origin} (numeric), \code{pop_destination} (numeric), \code{lat_origin} (numeric), \code{long_origin} (numeric),
#' \code{lat_destination} (numeric) and \code{long_destination} (numeric) or a \code{SpatialPolygonsDataFrame} object
#' 
#' @param \dots further arguments passed to or from other methods.
#' 
#' @return A data.frame containing location data with columns \code{location} (character), \code{population} (numeric), 
#' \code{x} (numeric) and \code{y} (numeric).
#' @name as.locationdataframe
#' @export
as.locationdataframe <- function(input, ...) {
  UseMethod("as.locationdataframe", input)
}

#' @rdname as.locationdataframe
#' @export
#' @method as.locationdataframe data.frame
as.locationdataframe.data.frame <- function(input, ...) {
  input <- input[!duplicated(input$origin),]
  pop <- as.numeric(input["pop_origin"]$pop_origin)
  lat <- as.numeric(input["lat_origin"]$lat_origin)
  long <- as.numeric(input["long_origin"]$long_origin)
  locations <- as.numeric(input["origin"]$origin)
  ans  <- data.frame(location = locations,
                     population = pop,
                     x = lat,
                     y = long)
  class(ans)  <- c('locationdataframe', 'data.frame')
  return (ans)
}

# use region data downloaded from http://www.gadm.org/country along with a world population raster
# make sure it is cropped to the correct region first using raster::crop
# for portugal, this works: crop(gadm, extent(-10, -6.189142, 30, 42.154232))
# portugal gadm is missing 2 municipalities (Tavira and Guimaraes): http://www.igeo.pt/DadosAbertos/Listagem.aspx#
#' @rdname as.locationdataframe
#' @param populationraster a \code{RasterLayer} object with each the value of
#'  each cell giving the associated population density.
#' @export
#' @method as.locationdataframe SpatialPolygonsDataFrame
as.locationdataframe.SpatialPolygonsDataFrame <- function(input, populationraster, ...) {
  result <- data.frame(simplifytext(input$NAME_2),input$ID_2,raster::extract(populationraster,input, fun=sum),sp::coordinates(input))
  colnames(result) <- c("name", "location", "population", "x", "y")
  class(result)  <- c('locationdataframe', 'data.frame')
  return (result)
}

#' @title  Check if given data.frame is 'locationdataframe' object
#'
#' @description
#' Helper function checking that the given object inherits from \code{locationdataframe} class. 
#' 
#' @param x The object to be checked.
#' 
#' @return True if the given object is a \code{locationdataframe} object; false otherwise
#' @export
is.locationdataframe <- function(x) {
  res  <- inherits(x, 'locationdataframe')
  return(res)
}

#' Standardise a text string to upper case ASCII with no spaces
#'
#' @param string The input string to simplify
#' @return A standardised text string.
#' @export
simplifytext <- function(string) {
  return (gsub("\\s", "_", toupper(iconv(string, from='UTF-8', to='ASCII//TRANSLIT'))))
}

#' Correlate the regions in a location dataframe with a list of regions which
#' map to an observed movement dataset.
#'
#' A utility function that creates a list containing a location dataframe and
#' a movement matrix. The \code{regionlist} and \code{movementdata} should be
#' from the same source, i.e. the IDs in the \code{movementdata} correspond to
#' the IDs in the regionlist. The data \code{location} is likely to be an
#' external datasource, and the locations may not precisely match those in the
#' \code{regionlist}. This function removes items from \code{location} which
#' don't exist in \code{regionlist} and vice-versa. It then converts
#' \code{movementdata} to a movement matrix with named rows and columns (based
#' on \code{regionlist}).
#'
#' @param location A \code{data.frame} containing "name", "location", "pop",
#' "lon" and "lat".
#' @param regionlist A \code{data.frame} containing "name" and "id"
#' @param movementdata A \code{data.frame} containing "origin", "destination",
#' "movement"
#' @return A \code{list} containing a \code{locations} \code{data.frame} with
#' "name", "lat", "lon" and "pop" fields, and a \code{observed} \code{matrix}
#' containing a movement matrix.
#' @export
correlateregions <- function(location, regionlist, movementdata) {
  if(!is(location, "data.frame")) {
    stop ("Parameter 'location' must be a data.frame!")
  }
  if(!is(regionlist, "data.frame")) {
    stop ("Parameter 'regionlist' must be a data.frame!")
  }
  if(!is(movementdata, "data.frame")) {
    stop ("Parameter 'movementdata' must be a data.frame!")
  }
  allnames <- as.vector(location$name)
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
  allnames <- location[allpresentindata,]
  
  if(length(datanames[,1]) != length(allnames[,1])) {
    stop("Something is wrong with the data provided. The number of regions found doesn't match!\n")
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
    originlocation <- (as.character(regionlist[regionlist$V1 == originaloriginid,2]))
    
    originaldestinationid <- rowdata$destination
    destinationlocation <- (as.character(regionlist[regionlist$V1 == originaldestinationid,2]))
    
    neworiginid <- (as.character(allnames[allnames$name == originlocation,2]))
    newdestinationid <- (as.character(allnames[allnames$name == destinationlocation,2]))
    
    movementdata[idx,1] <- originlocation
    movementdata[idx,2] <- destinationlocation
    movementdata[idx,3] <- as.numeric(movementdata[idx,3])
  }
  
  allnames <- allnames[order(allnames[,1]),]
  # code to remove the missing factors
  allnames$name <- as.factor(as.vector(allnames$name))
  
  return (list(locations=allnames[,c(1,3,4,5)],observed=as.movementmatrix(movementdata)))	
}

#' Show a plot comparing and optimised model, and an observed dataset.
#'
#' Plots the observed movement matrix, predicted movement matrix and the
#' difference between the two.
#'
#' @param optimisedmodel An \code{OptimisedModel} object containing a trained
#' dataset.
#' @param observed An observed movement matrix
#' @export
showcomparisonplot <- function(optimisedmodel, observed) {
  par(mfrow=c(2,2))
  plot(raster::raster(observed), main="Observed movement matrix")
  plot(raster::raster(optimisedmodel$trainingresults$prediction), main="Predicted movement matrix")
  plot(raster::raster(observed - optimisedmodel$trainingresults$prediction), main="Difference")
}

# @export
resultasdataframe <- function(predictedresult) {
  locations = predictedresult$df_locations
  mm = predictedresult$movement_matrix
  
  if (nrow(locations) != nrow(mm) ||  nrow(mm) != ncol(mm)) {
    stop("Something is wrong with this predicted result. The dimensions of the square matrix should be of the same length as the list of locations")
  }
  
  result <- data.frame(matrix(nrow=(nrow(mm)^2),ncol=3))
  
  for(idx in 1:nrow(mm)) {
    for(idx2 in 1:ncol(mm)) {
      row <- c(as.vector(locations[idx,1]),as.vector(locations[idx2,1]),mm[idx,idx2])
      rownum <- ((idx -1) * nrow(mm)) + idx2
      result[rownum,] <- row
    }
  }
  #head(result)
  colnames(result) <- c("Origin", "Destination", "Movement")
  return (result)
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
#' sp::plot(kenya)
#'
#' @references
#' Linard C., Gilbert, M. Snow, R.W., Noor, A.M. & Tatem, A.J. (2010)
#' Population Distribution, Settlement Patterns and Accessibility across
#' Africa in 2010. PLoS ONE
#' \url{http://www.plosone.org/article/info:doi/10.1371/journal.pone.0031743}
#' @name kenya
NULL

#' @name travelTime
#' @title Calculate travel times between coordinates
#' @description Using a friction raster (giving time taken to cross each cell)
#'  calculate the time taken to travel between pairs of coordinates.
#' @details This is a thin wrapper around functionality in the \code{gdistance}
#'  R package to facilitate the constuction of travel time matrices for use in
#'  movement models.
#' @param friction a \code{RasterLayer} object with cell values giving the time
#'  taken to traverse that cell.
#' @param coords a two-column matrix or dataframe, or a SpatialPoints object,
#'  giving coordinates to compute travel time from. If \code{coords2 = NULL},
#'  the a square matrix will be returned, giving travel time between these pairs
#'  of coordinates
#' @param coords2 an optional two-column matrix or dataframe, or a SpatialPoints
#'  object, giving coordinates to compute travel time to.
#' @param directions directions in which cells are connected, can be either 4,
#'  8, 16 or some other number. See \code{\link[gdistance]{adjacent}} for
#'  details
#' @param \dots additional arguments to pass to
#'  \code{\link[gdistance]{transition}}.
#'  
#' @return a matrix, with dimensions \code{nrow(coords), nrow(coords)}
#'  (If \code{coords2 = NULL}) or dimensions \code{nrow(coords), nrow(coords2)}
#'  otherwise
#' @importFrom gdistance transition costDistance
#' @export
#' @examples
#' # create a dummy friction matrix, assuming travel time is
#' # inversely proportional to population density
#' data(kenya)
#' kenya10 <- raster::aggregate(kenya, 10, sum)
#' friction <- 1 / kenya10
#' 
#' # example coordinates
#' coords <- matrix(c(38, 37, 36, 1, 0, 2), ncol = 2)
#' 
#' # calculate travel time between them
#' tt <- travelTime(friction, as.data.frame(coords))
#' tt
travelTime <- function (friction,
                        coords,
                        coords2 = NULL,
                        directions = 8,
                        ...) {
  
  # get self-distances if not specified
  if (is.null(coords2)) coords2 <- coords
  
  # coerce from dataframes to matrics
  if (is.data.frame(coords)) coords <- as.matrix(coords)
  if (is.data.frame(coords2)) coords2 <- as.matrix(coords2)
  
  # generate transition object
  tr <- transition(x = friction,
                   transitionFunction = function(x) {1 / mean(x)},
                   directions = directions)
  
  # get distance matrix
  access_distance <- costDistance(tr,
                                  coords,
                                  coords2)
  
  return (access_distance)
}

#####################################################
# variable transformations
#####################################################

# @name transformFluxObjectParameters
# @title Transform a list of parameters with the given transformations
# @description Using the list of transformation specified to transform a list of
# parameters. If the inverse is set to 'false' the transformation will be performed.
# Otherwise, the inverse transformation will be peformed. 
# @Note: the order of the parameters and their associated transformations must be 
# equivalent
# @param params a list of parameters
# @param transform a list of transformations for the parameters
# @param inverse if true makes the transformation; otherwise perform the inverse 
#   transformation. Default value is set to 'false'
# @return a list of transformed parameters
transformFluxObjectParameters  <- function(params, transform, inverse = FALSE){
  
  numberOfParams  <- length(params)
  transformedParams  <- vector(mode = "numeric", numberOfParams)
    
  for(i in 1:numberOfParams){
    transformation  <- transform[[i]]
    transformedParams[i]  <- transformation(params[[i]], inverse)
  }
  
  return (transformedParams)
}

# using the logarithm to ensure that any positive constraint values
# are unconstraint for the optimisation process
logTransform  <- function(x, inverse = FALSE){
  
  if(inverse){
    trans  <- exp(x)
  } else {
    trans  <- log(x)
  } 
  return (trans)
}

# using the 'probit transformation' to ensure that a variable which
# is constraint between [0,1] is unconstraint for the optimisation process
unitTransform  <- function(x, inverse = FALSE){

  if(inverse){
    trans  <- plogis(x)
  } else {
    trans  <- qlogis(x)
  } 
  return (trans)  
}

# no transformation required; simple return the input variable 
identityTransform  <- function(x, inverse = FALSE){  
  return (x) 
}
