# @title
#   Extracts the best individual of a set of individuals.
#
# @param population [\code{setOfIndividuals}]\cr
#  Population.
# @return [list]
#  List with the following entries
#  \describe{
#    \item{individual}{Genotype of the best individual.}
#    \item{fitness}{Fitness of the individual.}
#  }
getBestIndividual = function(population) {
  fitness = population$fitness
  best.idx = getMinIndex(fitness)
  return(
    list(
      individual = population$individuals[[best.idx]],
      fitness = fitness[best.idx]
    )
  )
}
