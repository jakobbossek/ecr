# Helper function which selects the individuals of a population which
# will survive the current generation.
#
# @param population [\code{setOfIndividuals}]\cr
#   Population.
# @param offspring [\code{integer(1)}]\cr
#   Generated offspring.
# @param population.size [\code{integer(1)}]\cr
#   Number of individuals.
# @param strategy [\code{character(1)}]\cr
#   Strategy used for selection. Possible strategies are:
#   \describe{
#     \item{plus}{A classical (mu + lambda) strategy.}
#     \item{comma}{A classical (mu, lambda) strategy.}
#   }
#   Default is \code{plus}. Another is not implemented yet.
# @param elite.size [\code{integer(1)}]\cr
#   Number of fittest individuals of the current generation that shall be copied to the
#   next generation without changing. Default is 0.
# @return [\code{setOfIndividuals}]
selectForSurvival = function(population, offspring, population.size, strategy = "plus", elite.size = 0L) {
  elite = NA

  if (strategy == "plus") {
    source.population = mergePopulations(population, offspring)
    source.individuals = source.population$population
    source.fitness = source.population$fitness
    to.survive = order(source.fitness)[seq(population.size)]
  } else if (strategy == "comma") {
    source.population = offspring
    source.individuals = source.population$population
    source.fitness = source.population$fitness
    if (elite.size > 0L) {
      #catf("Elitism with %i candidates out of %i", elite.size, population.size)
      parent.individuals = population$population
      parent.fitness = population$fitness
      to.be.elite = order(parent.fitness)[seq(elite.size)]
      # Adapt number of individuals taken from the offspring
      population.size = population.size - elite.size
      elite = makePopulation(
        individuals = parent.individuals[to.be.elite, , drop = FALSE],
        fitness = parent.fitness[to.be.elite]
      )
    }
    to.survive = order(source.fitness)[seq(population.size)]
  }

  population2 = makePopulation(
    individuals = source.individuals[to.survive, , drop = FALSE],
    fitness = source.fitness[to.survive]
  )

  # merge populations if elitism was used
  if (inherits(elite, "setOfIndividuals")) {
    population2 = mergePopulations(population2, elite)
  }
  return(population2)
}
