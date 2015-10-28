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
#' @seealso \code{\link{movement}} \code{\link{predict.flux}}, \code{\link{predict.optimisedmodel}}
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