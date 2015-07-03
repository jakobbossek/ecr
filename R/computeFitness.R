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
  # FIXME: fitness computation for non-list will be unnecessary
  if (testList(population$individuals[[1]])) {
    fitness = lapply(population$individuals, function(ind) do.call(fitness.fun, ind))
  } else {
    fitness = lapply(population$individuals, fitness.fun)
  }
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  return(fitness)
}
