#' @title
#' Performs survival selection.
#'
#' @description
#' Given the current optimization state and the list of offspring individuals,
#' the function selects the next generation. The selection type is based on the
#' parameters given in the control object.
#'
#' @param opt.state [\code{ecr_opt_state}]\cr
#'   Optimization state.
#' @param offspring [\code{ecr_population}]\cr
#'   Generated offspring.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @return [\code{ecr_population}]
#' @export
getNextGeneration = function(opt.state, offspring, control) {
  # extract info
  n.elite = control$n.elite
  strategy = control$survival.strategy
  n.population = control$n.population
  population = opt.state$population
  task = opt.state$task

  # init variables
  elite = NULL
  new.population = NULL

  # we need to distinguish the strategy here
  if (strategy == "plus") {
    # this is the easy case
    source.population = mergePopulations(population, offspring)
    new.population = selectForSurvival(opt.state, source.population, control)
  } else if (strategy == "comma") {
    # here we need to invest more effort since elitism might be activated
    source.population = offspring

    if (n.elite > 0L) {
      parent.individuals = population$individuals
      parent.fitness = population$fitness
      to.be.elite = order(parent.fitness)[seq(n.elite)]

      # get elite individuals
      elite.population = makePopulation(
        individuals = parent.individuals[to.be.elite],
        fitness = parent.fitness[to.be.elite]
      )

      # Adapt number of individuals taken from the offspring and select non-elite individuals
      n.population = n.population - n.elite
    }
    new.population = selectForSurvival(opt.state, source.population, control, n.select = n.population)
    if (n.elite > 0L) {
      new.population = mergePopulations(new.population, elite.population)
    }
  }
  return(new.population)
}

subsetPopulation = function(population, idx) {
  population$individuals = population$individuals[idx]
  population$fitness = population$fitness[, idx, drop = FALSE]
  return(population)
}
