#' Computes the fitness values for each member of a given population.
#'
#' This function gets a \code{setOfIndividuals}, computes the fitness and
#' extends the set of individuals (a list) with the fitness element.
#'
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @return [\code{setOfIndividuals}].
computeFitness = function(setOfIndividuals, fitness.fun) {
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  fitness.values = apply(setOfIndividuals$population, 1, fitness.fun)
  setOfIndividuals$fitness = fitness.values
  return(setOfIndividuals)
}