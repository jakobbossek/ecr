# @title
# Extracts the best individual of a set of individuals.
#
# @param population [\code{ecr_population}]\cr
#   Population.
# @return [list]
#  List with the following entries
#  \describe{
#    \item{individual}{Genotype of the best individual.}
#    \item{fitness}{Fitness of the individual.}
#  }
getBestIndividual = function(x, ...) {
  UseMethod("getBestIndividual")
}

getBestIndividual.ecr_opt_state = function(x) {
  getBestIndividual(x$population, x$task)
}

getBestIndividual.ecr_population = function(x, task) {
  fitness = x$fitness
  getIndex = ifelse (task$minimize, getMinIndex, getMaxIndex)
  best.idx = getIndex(fitness)
  return(
    list(
      individual = x$individuals[[best.idx]],
      fitness = fitness[best.idx]
    )
  )
}
