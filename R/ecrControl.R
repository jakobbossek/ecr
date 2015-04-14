#' Generates control object.
#'
#' The ecr package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple but powerful
#' wrapper for all these options and sets convenient default options.
#'
#' @param n.population [\code{integer(1)}]\cr
#'   Number of individuals in the population.
#' @param n.offspring [\code{integer(1)}]\cr
#'   Number of individuals generated in each generation.
#' @param n.mating.pool [\code{integer(1)}]\cr
#'   Number of individuals which can potentially participate in the
#'   generation of offspring. Default is half of the population size.
#' @param representation [\code{character(1)}]\cr
#'   Genotype representation of the parameters. Available are binary, real, integer and
#'   permutation.
#' @param survival.strategy [\code{character(1)}]\cr
#'   Determines the survival strategy used by the EA. Possible are 'plus' for a classical
#'   (mu + lambda) strategy and 'comma' for (mu, lambda).
#' @param n.elite [\code{integer(1)}]\cr
#'   Number of fittest individuals of the current generation that shall be copied to the
#'   next generation without changing. Default is 0. Keep in mind, that the algorithm
#'   does not care about this option if the \code{survival.strategy} is set to 'plus'.
#' @param n.params [\code{integer(1)}]\cr
#'   Number of parameters of the objective function.
#' @param target.name [\code{character(1)}]\cr
#'   Name for the objective fun values. Default is \dQuote{y}.
#' @param save.population.at [\code{integer}]\cr
#'   Which populations should be saved? Default is none.
#' @param monitor [\code{function}]\cr
#'   Monitoring function. Default is \code{consoleMonitor}.
#' @param stopping.conditions [\code{list}]\cr
#'   List of functions of type \code{ecr_stoppingCondition}.
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
  n.params,
  target.name = "y",
  save.population.at = integer(0),
  monitor = makeConsoleMonitor(),
  stopping.conditions = list()) {
  assertCount(n.population, positive = TRUE, na.ok = FALSE)
  assertCount(n.offspring, positive = TRUE, na.ok = FALSE)
  n.mating.pool = convertInteger(n.mating.pool)
  assertCount(n.mating.pool, positive = TRUE, na.ok = FALSE)
  assertChoice(representation, choices = getAvailableRepresentations())
  assertChoice(survival.strategy, choices = c("plus", "comma"))
  assertCount(n.elite, na.ok = FALSE)
  assertCount(n.params, positive = TRUE, na.ok = FALSE)
  assertCharacter(target.name, len = 1L, any.missing = FALSE)

  if (length(save.population.at) > 0) {
    assertInteger(save.population.at, lower = 0L, any.missing = FALSE)
  }

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

  structure(list(
    n.population = n.population,
    n.offspring = n.offspring,
    n.mating.pool = n.mating.pool,
    representation = representation,
    survival.strategy = survival.strategy,
    n.elite = n.elite,
    n.params = n.params,
    n.targets = NULL, # we set this by hand here
    save.population.at = save.population.at,
    target.name = target.name,
    stopping.conditions = stopping.conditions,
    monitor = monitor),
  class = "ecr_control")
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
  catf("Number of parameters         : %i", x$n.params)
  if (!is.null(x$n.targets)) {
    catf("Number of targets            : %i", x$n.targets)
  }
  catf("")

  catf("Evolutionary parameters:")
  catf("Population size              : %i", x$individuals.size)
  catf("Offspring size               : %i", x$n.offspring)
  catf("Mating pool size             : %i", x$n.mating.pool)
  catf("Representation               : %s", x$representation)
  catf("Survival strategy            : %s", if (x$survival.strategy == "plus") "(mu + lambda)" else "(mu, lambda)")
  if (x$n.elite > 0L && x$survival.strategy == "comma") {
    catf("(Using elitism with elite count %i, i.e., %.2g%% of the fittest candidates in each generation will survive)", x$n.elite, as.numeric(x$n.elite)/x$individuals.size)
  }

  catf("")
  catf("Evolutionary operators:")
  catf("Generator object             : %s", getOperatorName(x$generator))
  catf("Mutation operator            : %s (%s)", getOperatorName(x$mutator), getParametersAsString(x$mutator.control))
  catf("Recombination operator       : %s (%s)", getOperatorName(x$recombinator), getParametersAsString(x$recombinator.control))
}

getParametersAsString = function(parameters) {
  x = ""
  n = length(parameters)
  if (n == 0) {
    return("no parameters")
  }
  for (i in seq(n)) {
    name = names(parameters)[i]
    x = paste(x, " ", name, ": ", parameters[[name]], sep = "")
    if (i < n) {
      x = paste(x, ",", sep = "")
    }
  }
  return(x)
}
