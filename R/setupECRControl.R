#' @title
#' Generates control object.
#'
#' @description
#' The ecr package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple but powerful
#' wrapper for all these options and sets convenient defaults.
#'
#' @param n.population [\code{integer(1)}]\cr
#'   Number of individuals in the population.
#' @param n.offspring [\code{integer(1)}]\cr
#'   Number of individuals generated in each generation.
#' @param n.mating.pool [\code{integer(1)}]\cr
#'   Number of individuals which can potentially participate in the
#'   generation of offspring. Default is half of the population size.
#' @param representation [\code{character(1)}]\cr
#'   Genotype representation of the parameters. Available are binary, real,
#'   permutation and custom.
#' @param survival.strategy [\code{character(1)}]\cr
#'   Determines the survival strategy used by the EA. Possible are 'plus' for a classical
#'   (mu + lambda) strategy and 'comma' for (mu, lambda).
#' @param n.elite [\code{integer(1)}]\cr
#'   Number of fittest individuals of the current generation that shall be copied to the
#'   next generation without changing. Default is 0. Keep in mind, that the algorithm
#'   does not care about this option if the \code{survival.strategy} is set to 'plus'.
#' @param monitor [\code{function}]\cr
#'   Monitoring function. Default is \code{consoleMonitor}.
#' @param stopping.conditions [\code{list}]\cr
#'   List of functions of type \code{ecr_stoppingCondition}.
#' @param logger [\code{function}]\cr
#'   Monitoring object used to log stuff. Default is \code{defaultLogger} which
#'   logs the entire population and additional statistics like the minimal/maximal
#'   fitness values.
#' @param custom.constants [\code{list}]\cr
#'   Additional constants which should be available to all generators and operators.
#'   Defaults to empty list.
#' @param vectorized.evaluation [\code{logical(1L)}]\cr
#'   Is the fitness/objective function vectorized? I.e., does the fitness function accept
#'   a list? This allows for faster execution or parallelization by hand.
#'   If \code{TRUE} the following destinction on the type of the objective function is made:
#'   \describe{
#'     \item{Is \code{smoof_function}}{If the objective function is of type \code{smoof_function} from package \pkg{smoof}
#'     and the smoof function is vectorized, the population - which is a list internally -
#'     is reduced to a matrix and passed to the smoof function (vectorization in smoof
#'     is allowed for continuous functions only).}
#'     \item{Is not a \code{smoof_function}}{In this case the individuals of
#'     the population are passed entirely as a list to the objective function.}
#'   }
#'   Default is \code{FALSE}.
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
  monitor = makeConsoleMonitor(),
  stopping.conditions = list(),
  logger = makeNullMonitor(),
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
      inherits(condition, "ecr_stoppingCondition")
    })
    if (any(!valid)) {
      stopf("All stopping conditions need to have type 'ecr_stoppingCondition'.")
    }
  }

  if (!inherits(monitor, "ecr_monitor")) {
    stopf("Currently only monitor of type 'ecr_monitor' supported")
  }

  event.dispatcher = setupEventDispatcher()
  if (!is.null(monitor)) {
    event.dispatcher$registerAction("onEAInitialized", monitor$before)
    event.dispatcher$registerAction("onPopulationUpdated", monitor$step)
    event.dispatcher$registerAction("onEAFinished", monitor$after)
  }

  if (!is.null(logger)) {
    event.dispatcher$registerAction("onEAInitialized", logger$before)
    event.dispatcher$registerAction("onPopulationUpdated", logger$step)
    event.dispatcher$registerAction("onEAFinished", logger$after)
  }

  ctrl = makeS3Obj(
    n.population = n.population,
    n.offspring = n.offspring,
    n.mating.pool = n.mating.pool,
    representation = representation,
    survival.strategy = survival.strategy,
    n.elite = n.elite,
    stopping.conditions = stopping.conditions,
    monitor = monitor,
    logger = logger,
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
  catf("[ecr CONTROL OBJECT]\n")

  catf("Objective function:")
  if (is.null(x$n.targets)) {
    catf("Optimizing mono-criteria objective function.")
  } else {
    catf("Optimizing multi-criteria objective function (%i targets).", x$n.targets)
  }
  # catf("Number of parameters         : %i", x$n.params)
  # if (!is.null(x$n.targets)) {
  #   catf("Number of targets            : %i", x$n.targets)
  # }
  # catf("")

  catf("Evolutionary parameters:")
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

  catf("")
  catf("Evolutionary operators:")
  catf("Generator object             : %s", getOperatorName(x$generator))
  catf("Mutation operator            : %s (%s)", getOperatorName(x$mutator),
    getParametersAsString(getOperatorParameters(x$mutator))
  )
  catf("Recombination operator       : %s (%s)", getOperatorName(x$recombinator),
    getParametersAsString(getOperatorParameters(x$recombinator))
  )
}

recombine = function(parents, task, control) {
  control$recombinator(parents, task, control)
}

mutate = function(parent, task, control) {
  control$mutator(parent, task, control)
}

selectForMating = function(opt.state, control) {
  population = opt.state$population
  task = opt.state$task
  fitness = population$fitness
  fitness2 = transformFitness(fitness, task, control$parent.selector)
  idx.mating = control$parent.selector(fitness2, control$n.mating.pool, task, control, NULL)
  subsetPopulation(population, idx = idx.mating)
}

selectForSurvival = function(opt.state, population, control, n.select = control$n.population) {
  n.population = control$n.population
  fitness = population$fitness
  task = opt.state$task
  fitness2 = transformFitness(fitness, task, control$survival.selector)
  idx.survival = control$survival.selector(fitness2, n.population, task, control, NULL)
  subsetPopulation(population, idx = idx.survival)
}

# @title
# Fitness transformation / scaling.
#
# @description
# Some selectors support maximization only, e.g., roulette wheel selector, or
# minimization (most others). This function computes a factor from {-1, 1} for
# each objective to match supported selector optimization directions and
# the actual objectives of the task.
#
# @param fitness [matrix]
#   Matrix of fitness values with the fitness vector of individual i in the i-th
#   column.
# @param task [ecr_optimization_task]
#   Optimization task.
# @param control [ecr_control]
#   Control object.
# @return [matrix] Transformed / scaled fitness matrix.
transformFitness = function(fitness, task, selector) {
  # logical vector of opt directions
  task.dir = task$minimize
  # "vectorize" character indicating supported opt direction by selector
  sup.dir = rep(attr(selector, "supported.opt.direction"), task$n.objectives)
  # "logicalize" selector opt direction
  sup.dir = (sup.dir == "minimize")

  fn.scale = ifelse(xor(task.dir, sup.dir), -1, 1)

  # build transformation matrix
  fn.scale = if (task$n.objectives == 1L) {
    #FIXME: R BUG?!?!
    # diag(ifelse(xor(task.dir, sup.dir), -1, 1)) breaks with message
    # Fehler in diag(ifelse(xor(task.dir, sup.dir), -1, 1)) : ung"ultiger 'nrow' Wert (< 0)
    # if n.objectives is 1! -.-
    # Weird R bug??? diag(1) works!
    as.matrix(fn.scale)
  } else {
    diag(fn.scale)
  }
  # transform fitness
  return(fn.scale %*% fitness)
}
