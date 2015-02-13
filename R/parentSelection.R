# Helper function for creating a mating pool.
#
# @param population [\code{setOfIndividuals}]\cr
#   Population.
# @param number.of.parents [\code{integer(1)}]\cr
#   Number of parents which shall be selected for the mating pool.
# @return [\code{setOfIndividuals}]
parentSelection = function(population, number.of.parents) {
  individuals = population$individuals
  fitness = population$fitness
  to.keep = order(fitness)[seq(number.of.parents)]
  makePopulation(individuals[to.keep, , drop = FALSE], fitness[to.keep])
}
