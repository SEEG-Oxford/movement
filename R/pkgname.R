#' @title Modelling and Analysing Movement Data for Epidemiology
#'
#' @description Movement of humans and animals has a crucial role in the epidemiology of a
#' number of diseases. Movement data is increasingly available to
#' epidemiologists and its incorporation in models and maps of disease is
#' increasingly popular. This package is a collaborative effort to improve our
#' ability to analyses movement data and to build and apply epidemiological
#' movement models.
#'
#' @docType package
#' @name movement-package
#'
#' @details
#' \tabular{ll}{
#' Package: \tab movement\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-08-08\cr
#' License: \tab GPL (>=2)\cr
#' }
#'
#' @seealso \code{\link{movementmodel}}, \code{\link{predict}},
#' \code{\link{showprediction}}, \code{\link{movement.predict}},
#' \code{\link{get.network}}, \code{\link{kenya}}, \code{\link{gravity.flux}},
#' \code{\link{continuum.flux}}, \code{\link{attemptoptimisation}}
#'
#' @author
#' Nick Golding, Andrew Schofield, Moritz Kraemer and Alex T. Perkins
#' Maintainer: Nick Golding <nick.golding.research at gmail.com>
#'
#' @examples
#' # load kenya raster
#' data(kenya)
#' # aggregate to 10km to speed things up
#' kenya10 <- raster::aggregate(kenya, 10, sum)
#' # create the prediction model for the aggregate dataset using the fixed parameter radiation model
#' predictionModel <- movementmodel(dataset=kenya10,
#'                                  min_network_pop = 50000,
#'                                  predictionmodel= 'original radiation',
#'                                  symmetric = TRUE,
#'                                  modelparams = 0.1)
#' # predict the population movement from the model
#' predictedMovements = predict(predictionModel)
#' # visualise the distance matrix
#' sp::plot(raster::raster(predictedMovements$net$distance_matrix))
#' # visualise the predicted movements overlaid onto the original raster
#' showprediction(predictedMovements)
NULL