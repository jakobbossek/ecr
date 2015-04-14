# Helper function which selects the individuals of a population which
# will survive the current generation.
#
# @param population [\code{setOfIndividuals}]\cr
#   Population.
# @param offspring [\code{integer(1)}]\cr
#   Generated offspring.
# @param n.population [\code{integer(1)}]\cr
#   Number of individuals.
# @param strategy [\code{character(1)}]\cr
#   Strategy used for selection. Possible strategies are:
#   \describe{
#     \item{plus}{A classical (mu + lambda) strategy.}
#     \item{comma}{A classical (mu, lambda) strategy.}
#   }
#   Default is \code{plus}. Another is not implemented yet.
# @param n.elite [\code{integer(1)}]\cr
#   Number of fittest individuals of the current generation that shall be copied to the
#   next generation without changing. Default is 0.
# @return [\code{setOfIndividuals}]
selectForSurvival = function(population, offspring, n.population, strategy = "plus", n.elite = 0L) {
  elite = NULL
  if (strategy == "plus") {
    source.population = mergePopulations(population, offspring)
    source.individuals = source.population$individuals
    source.fitness = source.population$fitness
    to.survive = order(source.fitness)[seq(n.population)]
  } else if (strategy == "comma") {
    source.population = offspring
    source.individuals = source.population$individuals
    source.fitness = source.population$fitness
    if (n.elite > 0L) {
      #catf("Elitism with %i candidates out of %i", n.elite, n.population)
      parent.individuals = population$individuals
      parent.fitness = population$fitness
      to.be.elite = order(parent.fitness)[seq(n.elite)]
      # Adapt number of individuals taken from the offspring
      n.population = n.population - n.elite
      elite = makePopulation(
        individuals = parent.individuals[to.be.elite],
        fitness = parent.fitness[to.be.elite]
      )
    }
    to.survive = order(source.fitness)[seq(n.population)]
  }

  population2 = makePopulation(
    individuals = source.individuals[to.survive],
    fitness = source.fitness[to.survive]
  )

  # merge populations if elitism was used
  if (!is.null(elite)) {
    population2 = mergePopulations(population2, elite)
  }
  return(population2)
}
