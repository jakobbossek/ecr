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
#'   generation of offspring.
#'   Default is half of the population size.
#' @param representation [\code{character(1)}]\cr
#'   Genotype representation of the parameters. Available are \dQuote{binary},
#'   \dQuote{float}, \dQuote{permutation} and \dQuote{custom}.
#' @param survival.strategy [\code{character(1)}]\cr
#'   Determines the survival strategy used by the EA. Possible are \dQuote{plus} for
#'   a classical (mu + lambda) strategy and \dQuote{comma} for (mu, lambda).
#' @param n.elite [\code{integer(1)}]\cr
#'   Number of fittest individuals of the current generation that shall be copied to the
#'   next generation without changing. Keep in mind, that the algorithm
#'   does not care about this option if the \code{survival.strategy} is set to 'plus'.
#'   Default is 0.
#' @param monitor [\code{function}]\cr
#'   Monitoring function.
#'   Default is \code{NULL}, i.e. no monitoring.
#' @param stopping.conditions [\code{list}]\cr
#'   List of functions of type \code{ecr_stoppingCondition}. At least one stopping
#'   condition needs to be passed.
#'   Default is the empty list.
#' @param logger [\code{function}]\cr
#'   Monitoring object used to log stuff.
#'   Default is \code{NULL} which means no logging at all.
#'   See \code{\link{makeOptPathLoggingMonitor}} for ecr's build-in logger.
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
      inherits(condition, "ecr_stoppingCondition")
    })
    if (any(!valid)) {
      stopf("All stopping conditions need to have type 'ecr_stoppingCondition'.")
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

selectForMating = function(opt.state, control) {
  population = opt.state$population
  task = opt.state$task
  fitness = population$fitness
  fitness2 = transformFitness(fitness, task, control$parent.selector)
  idx.mating = control$parent.selector(fitness2, control$n.mating.pool, task, control, opt.state)
  subsetPopulation(population, idx = idx.mating)
}

selectForSurvival = function(opt.state, population, control, n.select = control$n.population) {
  n.population = control$n.population
  fitness = population$fitness
  task = opt.state$task
  fitness2 = transformFitness(fitness, task, control$survival.selector)
  idx.survival = control$survival.selector(fitness2, n.population, task, control, opt.state)
  subsetPopulation(population, idx = idx.survival)
}
