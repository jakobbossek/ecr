#' @import ParamHelpers
#' @import BBmisc
#' @import smoof
#' @import ggplot2
#' @import checkmate
#' @import parallelMap
#' @import reshape2
#' @import gridExtra
#' @useDynLib ecr
NULL

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "ecr", levels = c("evaluateFitness"))
}
