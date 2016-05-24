#' @title
#' Generates control object.
#'
#' @description
#' The ecr package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple
#' wrapper for all these options and sets convenient defaults.
#'
#' @template arg_n_population
#' @template arg_n_offspring
#' @template arg_n_mating_pool
#' @template arg_representation
#' @template arg_survival_strategy
#' @template arg_n_elite
#' @template arg_monitor
#' @param stopping.conditions [\code{list}]\cr
#'   List of functions of type \code{ecr_terminator}. At least one stopping
#'   condition needs to be passed.
#'   Default is the empty list.
#' @template arg_logger
#' @template arg_custom_constants
#' @template arg_vectorized_evaluation
#' @return
#'   S3 object of type \code{ecr_control}.
#' @export
setupECRControl = function(
  n.population,
  n.offspring,
  n.mating.pool = floor(n.population / 2),
  representation,
  survival.strategy = "plus",
  n.elite = 0L,
  monitor = setupConsoleMonitor(),
  stopping.conditions = list(),
  logger = NULL,
  custom.constants = list(),
  vectorized.evaluation = FALSE) {
  assertCount(n.population, positive = TRUE, na.ok = FALSE)
  assertCount(n.offspring, positive = TRUE, na.ok = FALSE)
  n.mating.pool = convertInteger(n.mating.pool)
  assertCount(n.mating.pool, positive = TRUE, na.ok = FALSE)
  assertChoice(representation, choices = getAvailableRepresentations())
  assertChoice(survival.strategy, choices = c("plus", "comma"))
  assertCount(n.elite, na.ok = FALSE)
  assertList(custom.constants, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
  assertFlag(vectorized.evaluation, na.ok = FALSE)

  # If the survival strategy is (mu + lambda), than the number of generated offspring in each iteration
  # must greater or equal to the population size
  if (survival.strategy == "comma" && n.offspring < n.population) {
    stopf("The (mu, lambda) survival strategy requires the number of generated offspring in each generation
      to be greater or equal to the population size, but %i < %i", n.offspring, n.population)
  }

  if (survival.strategy == "comma" && n.elite >= n.population) {
    stopf("n.elite must be smaller than n.population! Otherwise each population would be the same.")
  }

  if (length(stopping.conditions) == 0) {
    stopf("You need to specify at least one stopping condition.")
  } else {
    valid = sapply(stopping.conditions, function(condition) {
      inherits(condition, "ecr_terminator")
    })
    if (any(!valid)) {
      stopf("All stopping conditions need to have type 'ecr_terminator'.")
    }
  }

  event.dispatcher = setupEventDispatcher()
  if (!is.null(monitor)) {
    installMonitor(event.dispatcher, monitor)
  }

  if (!is.null(logger)) {
    installMonitor(event.dispatcher, logger)
  }

  ctrl = makeS3Obj(
    n.population = n.population,
    n.offspring = n.offspring,
    n.mating.pool = n.mating.pool,
    representation = representation,
    survival.strategy = survival.strategy,
    n.elite = n.elite,
    stopping.conditions = stopping.conditions,
    custom.constants = custom.constants,
    vectorized.evaluation = vectorized.evaluation,
    event.dispatcher = event.dispatcher,
    classes = "ecr_control"
  )

  # set defaults if one of the standard representations is used
  if (representation != "custom") {
    ctrl = setupEvolutionaryOperators(ctrl)
  }

  return(ctrl)
}

#' Print ecr control object.
#'
#' @param x [\code{ecr_control}]\cr
#'   Control object.
#' @param ... [any]\cr
#'   Not used.
#'
#' @export
print.ecr_control = function(x, ...) {
  catf("CONTROL OBJECT\n")

  catf("Parameters:")
  catf("Population size              : %i", x$n.population)
  catf("Offspring size               : %i", x$n.offspring)
  catf("Mating pool size             : %i", x$n.mating.pool)
  catf("Representation               : %s", x$representation)
  catf("Survival strategy            : %s", if (x$survival.strategy == "plus") "(mu + lambda)" else "(mu, lambda)")
  if (x$n.elite > 0L && x$survival.strategy == "comma") {
    catf("(Using elitism with elite count %i, i.e., %.2g%% of the fittest
      candidates in each generation will survive)",
    x$n.elite, as.numeric(x$n.elite)/x$n.population)
  }

  catf("\nOperators:")
  catf("Generator object             : %s", if (is.null(x$generator)) NA else getOperatorName(x$generator))
  mut.name = if (is.null(x$mutator)) NA else getOperatorName(x$mutator)
  mut.params = if (is.null(x$mutator)) NA else getParametersAsString(getOperatorParameters(x$mutator))
  catf("Mutation operator            : %s (%s)", mut.name, mut.params)

  rec.name = if (is.null(x$recombinator)) NA else getOperatorName(x$recombinator)
  rec.params = if (is.null(x$recombinator)) NA else getParametersAsString(getOperatorParameters(x$recombinator))
  catf("Recombination operator       : %s (%s)", rec.name, rec.params)
}

recombine = function(parents, task, control) {
  control$recombinator(parents, task, control)
}

mutate = function(parent, task, control) {
  control$mutator(parent, task, control)
}

#' @title
#' Generate mating pool.
#'
#' @description
#' Given the current optimization state this function selects a subset of individuals
#' which form the mating pool, i.e., the set of possible parents for later recombination.
#'
#' @note
#' Basically this is a wrapper for the call to the parent selector of the control
#' object which occasionally transforms the fitness values (e.g., if maximzation should be
#' performed, but the selection operator actualy minimizes).
#'
#' @template arg_opt_state
#' @template arg_control
#' @return [\code{ecr_population}]
#' @export
selectForMating = function(opt.state, control) {
  population = opt.state$population
  task = opt.state$task
  fitness = population$fitness
  fitness2 = transformFitness(fitness, task, control$parent.selector)
  idx.mating = control$parent.selector(fitness2, control$n.mating.pool, task, control, opt.state)
  subsetPopulation(population, idx = idx.mating)
}

#' @title
#' Generate mating pool.
#'
#' @description
#' Given the current optimization state this function selects a subset of individuals
#' which should survive and form the next generation.
#'
#' @note
#' Basically this is a wrapper for the call to the parent selector of the control
#' object which occasionally transforms the fitness values (e.g., if maximzation should be
#' performed, but the selection operator actualy minimizes).
#'
#' @template arg_opt_state
#' @param population [\code{list}]\cr
#'   Current population.
#' @template arg_control
#' @param n.select [\code{integer(1L)}]\cr
#'   Number of offspring to select for survival.
#' @return [\code{ecr_population}]
#' @export
selectForSurvival = function(opt.state, population, control, n.select = control$n.population) {
  n.population = control$n.population
  fitness = population$fitness
  task = opt.state$task
  fitness2 = transformFitness(fitness, task, control$survival.selector)
  idx.survival = control$survival.selector(fitness2, n.population, task, control, opt.state)
  subsetPopulation(population, idx = idx.survival)
}
