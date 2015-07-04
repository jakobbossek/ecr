#' Computes the fitness values for each member of a given population.
#'
#' This function gets a population computes the fitness and
#' returns the vector of fitness values.
#'
#' @param population [\code{matrix}]\cr
#'   Population.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @param control [\code{ecr_control}]\cr
#'   Control object containing all operators and further parameters.
#'   See \code{\link{setupECRControl}} and \code{\link{setupEvolutionaryOperators}}.
#' @return [\code{matrix}].
computeFitness = function(population, fitness.fun, control) {
  # FIXME: fitness computation for non-list will be unnecessary
  if (getParamNr(control$par.set) == 1L) {
    fitness = lapply(population$individuals, fitness.fun)
  } else {
    fitness = lapply(population$individuals, function(ind) do.call(fitness.fun, ind))
  }
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  return(fitness)
}
