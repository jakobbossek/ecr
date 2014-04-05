# Helper function which selects the individuals of a population which
# will survive the current generation.
#
# @param setOfIndividuals [\code{setOfIndividuals}]\cr
#   Population.
# @param pop.size [\code{integer(1)}]\cr
#   Number of individuals which shall be selected.
# @param strategy [\code{character(1)}]\cr
#   Strategy used for selection. Possible strategies are:
#   \describe{
#     \item{mupluslambda}{A classical (mu + lambda) strategy.}
#     \item{mucommalambda}{A classical (mu, lambda) strategy.}
#   }
#   Default is \code{mupluslambda}. Another is not implemented yet.
# @return [\code{setOfIndividuals}]
selectForSurvival = function(setOfIndividuals, pop.size, strategy = "mupluslambda") {
  #FIXME: (mu, lambda) is missing
  #FIXME: 'merging' population and offspring makes it impossible to determine
  #       from which individuals to choose in the (mu, lambda) strategy. Rethink this!
  #FIXME: elitism is missing ((mu + lambda) always lets the fittest survive, but for (mu, lambda)
  #       this should be kept in mind)
  individuals = setOfIndividuals$population
  fitness = setOfIndividuals$fitness
  to.survive = order(fitness)[seq(pop.size)]
  makePopulation(
    individuals = individuals[to.survive, ],
    fitness = fitness[to.survive]
    )
}
