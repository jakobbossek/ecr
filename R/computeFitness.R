#' Computes the fitness values for each member of a given population.
#'
#' This function gets a population computes the fitness and
#' returns the vector of fitness values.
#'
#' @param population [\code{matrix}]\cr
#'   Population.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @return [\code{matrix}].
computeFitness = function(population, fitness.fun) {
    fitness = unlist(lapply(population$individuals, fitness.fun))
    return(fitness)
}
