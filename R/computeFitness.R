# @title
# Computes the fitness values for each member of a given population.
#
# @description
# This function expects a population, computes the fitness and
# returns the matrix of fitness values. This function makes use of
# \code{\link[parallelMap]{parallelMap}} to allow parallelization of fitness
# evaluation.
#
# @param population [\code{matrix}]\cr
#   Population.
# @param fitness.fun [\code{function}]\cr
#   Fitness function.
# @param task [\code{ecr_optimization_task}]\cr
#   Optimization task.
# @param control [\code{ecr_control}]\cr
#   Control object containing all operators and further parameters.
#   See \code{\link{setupECRControl}} and \code{\link{setupEvolutionaryOperators}}.
# @return [\code{matrix}].
computeFitness = function(population, fitness.fun, task, control) {
  if (getParamNr(task$par.set) == 1L) {
    # one parameter
    fitness = parallelMap(fitness.fun, population$individuals, level = "ecr.evaluateFitness")
  } else {
    # many parameters, which are explicit defined in function
    fitness = parallelMap(function(ind) do.call(fitness.fun, ind), population$individuals, level = "ecr.evaluateFitness")
  }
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  return(fitness)
}
