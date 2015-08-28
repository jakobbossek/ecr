#' @title
#'   Performs survival selection.
#'
#' @description
#'   Helper function which selects the individuals of a population which
#'   will survive the current generation.
#'
#' @param population [\code{setOfIndividuals}]\cr
#'   Population.
#' @param offspring [\code{integer(1)}]\cr
#'   Generated offspring.
#' @param STORAGE [\code{list}]\cr
#'   List which contains all the algorithm specific stuff.
#' @param task [\code{ecr_optimization_task}]\cr
#'   Optimization task.
#' @param n.population [\code{integer(1)}]\cr
#'   Number of individuals.
#' @param strategy [\code{character(1)}]\cr
#'   Strategy used for selection. Possible strategies are:
#'   \describe{
#'     \item{plus}{A classical (mu + lambda) strategy.}
#'     \item{comma}{A classical (mu, lambda) strategy.}
#'   }
#'   Default is \code{plus}. Another is not implemented yet.
#' @param n.elite [\code{integer(1)}]\cr
#'   Number of fittest individuals of the current generation that shall be copied to the
#'   next generation without changing. Default is 0.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @return [\code{setOfIndividuals}]
getNextGeneration = function(population, offspring, STORAGE, task, n.population, strategy = "plus", n.elite = 0L, control) {
  elite = NULL
  new.population = NULL
  if (strategy == "plus") {
    source.population = mergePopulations(population, offspring)
    new.population = selectForSurvival(control, source.population, STORAGE, task, n.population)
  } else if (strategy == "comma") {
    source.population = offspring
    elite = list()

    if (n.elite > 0L) {
      #catf("Elitism with %i candidates out of %i", n.elite, n.population)
      parent.individuals = population$individuals
      parent.fitness = population$fitness
      to.be.elite = order(parent.fitness)[seq(n.elite)]

      # get elite individuals
      elite = makePopulation(
        individuals = parent.individuals[to.be.elite],
        fitness = parent.fitness[to.be.elite]
      )

      # Adapt number of individuals taken from the offspring and select non-elite individuals
      n.population = n.population - n.elite
    }
    new.population = selectForSurvival(control, offspring, STORAGE, task, n.population)
    if (n.elite > 0L) {
      new.population = mergePopulations(new.population, elite)
    }
  }
  return(new.population)
}
