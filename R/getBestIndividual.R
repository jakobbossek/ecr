#' Extracts the best individual of a set of individuals.
#'
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'  Set of individuals (population).
#' @return [\code{essoIndividual}]
#'  Individual with best, i. e., lowest fitness value.
#' @export
getBestIndividual = function(setOfIndividuals) {
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  population = setOfIndividuals$population
  population.fitness = setOfIndividuals$fitness
  best.idx = which.min(population.fitness)
  best.fitness = population.fitness[best.idx]
  best.individual = population[best.idx, ]
  return(structure(
    list(fitness = best.fitness, individual = best.individual),
    class = "esooIndividual"))
}