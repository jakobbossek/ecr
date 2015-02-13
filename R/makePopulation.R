# Helper function for wrapping individuals in a 'setOfIndividuals'.
#
# @param [\code{matrix}]\cr
#   Matrix of individuals.
# @param [\code{numeric}]\cr
#   Vector of fitness values for the individuals.
# @return [\code{setOfIndividuals}]
makePopulation = function(individuals, fitness = NULL) {
  structure(
    list(
      individuals = individuals,
      fitness = fitness
    ),
    classes = c("ecrPopulation", "setOfIndividuals")
  )
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
    individuals = rbind(individuals, populations[[i]]$individuals)
    fitness = c(fitness, populations[[i]]$fitness)
  }
  makePopulation(
    individuals = as.matrix(individuals),
    fitness = fitness
  )
}
