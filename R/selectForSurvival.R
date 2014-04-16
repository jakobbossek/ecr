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
# @return [\code{setOfIndividuals}]
selectForSurvival = function(population, offspring, population.size, strategy = "plus", elitism = 0L) {
  if (strategy == "plus") {
    source.population = mergePopulations(population, offspring)
  } else {
    source.population = offspring
  }
  individuals = source.population$population
  fitness = source.population$fitness
  to.survive = order(fitness)[seq(population.size)]
  return(makePopulation(
    individuals = individuals[to.survive, , drop = FALSE],
    fitness = fitness[to.survive]
  ))
}
