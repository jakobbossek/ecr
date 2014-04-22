#' Simple monitoring function.
#'
#' This is the default monitoring function used by ecr. It simply outputs
#' the iteration as well as minimal, mean and maximal target values from the current
#' population.
#'
#' @param objective.fun [\code{function}]\cr
#'   Objective function.
#' @param population [\code{setOfIndividuals}]\cr
#'   Current population.
#' @param trace [\code{ecr_trace}]\cr
#'   Optimization trace.
#' @param iter [\code{integer(1)}]\cr
#'   Current iteration.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#'
#' @export
consoleMonitor = function(objective.fun, population, trace, iter, control) {
  max.iter = control$max.iter
  fitness = population$fitness
  if (control$n.targets == 1L) {
    catf("Iter %i | y (min: %0.2g, mean: %0.2g, max: %0.2g)", iter, min(fitness), mean(fitness), max(fitness))
  }
}
