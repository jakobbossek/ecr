#' @title
#'   Computes the fitness values for each member of a given population.
#'
#' @description
#'   This function expects a population, computes the fitness and
#'   returns the matrix of fitness values.
#'
#' @param population [\code{matrix}]\cr
#'   Population.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @return [\code{matrix}].
computeFitness = function(population, fitness.fun) {
  fitness = lapply(population$individuals, fitness.fun)
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  return(fitness)
}
