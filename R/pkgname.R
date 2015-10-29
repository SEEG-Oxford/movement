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
#' @note
#' The most common use of the package is to parameterize a movement model based on observed 
#' population movements, and then use this model to predict _de novo_ population movements.
#' Code to fit such a model might look like this:
#' 
#' \code{m <- movement(observed_movement ~ location_data, model = radiationWithSelection())}
#' 
#' where \code{observed_movement} is a \code{movement_matrix} object containing observations 
#' about movements between pairs of locations, \code{location_data} is a \code{location_dataframe} 
#' object containing the coordinates and populations of those locations, and \code{radiationWithSelection()}
#' creates a \code{flux} object, representing the type of movement model to by fitted. Current supported 
#' movement models are: \code{\link{radiationWithSelection}}, \code{\link{originalRadiation}}, 
#' \code{\link{gravity}}, \code{\link{gravityWithDistance}}, \code{\link{interveningOpportunities}} and
#' \code{\link{uniform selection}}.
#' 

#' The \code{\link{movement}} model fits the parameters of the specified movement model, and returns a 
#' \code{movement_model} object. This object can be plotted (\code{plot(m)}), or used to predict to 
#' populations movements to new \code{location_dataframe} object (\code{prediction <- predict(m, location_data)}), 
#' or even a \code{RasterLayer} object giving populations in each cell (\code{prediction <- predict(m, raster)}).
#'
#' @seealso \code{\link{movement}} \code{\link{predict.flux}}, \code{\link{predict.movement_model}}
#' \code{\link{showprediction}}, \code{\link{getNetwork}}, \code{\link{kenya}}, 
#'
#' @importFrom graphics arrows par plot points
#' @importFrom grDevices rgb
#' @importFrom methods is
#' @importFrom stats coef dist dpois naprint optim
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' 
#' @author
#' Nick Golding, Andrew Schofield, Moritz Kraemer and Alex T. Perkins
#' Maintainer: Nick Golding <nick.golding.research at gmail.com>
#'
NULL