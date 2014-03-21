# Helper function for creating a mating pool.
#
# @param setOfIndividuals [\code{setOfIndividuals}]\cr
#   Population.
# @param number.of.parents [\code{integer(1)}]\cr
#   Number of parents which shall be selected for the mating pool.
# @return [\code{setOfIndividuals}]
parentSelection = function(setOfIndividuals, number.of.parents) {
  #FIXME: maybe better call this 'createMatingPool' or so
  #FIXME: until now we select just the n elements with the lowest fitness values
  #       add fitness-proportional selection (rhoulette-wheel-selection), rang based
  #       selection procedures and more, i.e., write a closure which generates the selection function
  #       to keep the esoo function simple.
  stopifnot(inherits(setOfIndividuals, "setOfIndividuals"))
  individuals = setOfIndividuals$population
  fitness = setOfIndividuals$fitness
  to.keep = order(fitness)[seq(number.of.parents)]
  makePopulation(individuals = individuals[to.keep, ], fitness = fitness[to.keep])
}
