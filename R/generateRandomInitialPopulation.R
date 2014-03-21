#' Generates an initial population.
#'
#' Samples uniformally distributed points in the design space of the target function
#' taking care not to violate bounds.
#'
#' @param size [\code{integer(1)}]\cr
#'   Number of points to generate.
#' @param n.params [\code{integer(1)}]\cr
#'   Number of parameter of the target function.
#' @param lower.bounds [\code{numeric}]\cr
#'   Vector of size \code{n.params} indicating the lower bounds for each dimension.
#' @param upper.bounds [\code{numeric}]\cr
#'   Vector of size \code{n.params} indicating the upper bounds for each dimension.
#' @return [\code{setOfIndividuals}]
generateRandomInitialPopulation = function(size, n.params, lower.bounds, upper.bounds) {
  population = matrix(0, nrow = size, ncol = n.params)
  for (i in seq(n.params)) {
    population[, i] = runif(size, min = lower.bounds[i], max = upper.bounds[i])
  }
  makePopulation(population)
}

# Helper function for wrapping individuals in a 'setOfIndividuals'.
#
# @param [\code{matrix}]\cr
#   Matrix of individuals.
# @param [\code{numeric}]\cr
#   Vector of fitness values for the individuals.
# @return [\code{setOfIndividuals}]
makePopulation = function(individuals, fitness) {
  res = list(population = individuals)
  if (!missing(fitness))
    res$fitness = fitness
  #FIXME: do we really ne both esooPopulation and setOfIndividuals?
  structure(
    res,
    class = c("esooPopulation", "setOfIndividuals"))
}

# Helper for merging populations.
#
# @param individuals1, individuals2 [\code{setOfIndividuals}]\cr
#  The source sets of individuals.
# @return [\code{setOfIndividuals}]
mergePopulations = function(individuals1, individuals2) {
  makePopulation(
    individuals = rbind(individuals1$population, individuals2$population),
    fitness = c(individuals1$fitness, individuals2$fitness)
    )
}