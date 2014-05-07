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
  structure(
    res,
    class = c("ecrPopulation", "setOfIndividuals"))
}

# Helper for merging populations.
#
# @param .. [\code{list}]\cr
#  List of objects of type \code{setOfIndividuals}.
# @return [\code{setOfIndividuals}]
mergePopulations = function(...) {
  populations = list(...)
  individuals = data.frame()
  fitness = c()
  for (i in 1:length(populations)) {
    individuals = rbind(individuals, populations[[i]]$population)
    fitness = c(fitness, populations[[i]]$fitness)
  }
  makePopulation(
    individuals = as.matrix(individuals),
    fitness = fitness
  )
}
