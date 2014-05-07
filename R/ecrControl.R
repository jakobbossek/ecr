#' Generates control object.
#'
#' The ecr package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple but powerful
#' wrapper for all these options and sets convenient default options.
#'
#' @param population.size [\code{integer(1)}]\cr
#'   Number of individuals in the population.
#' @param offspring.size [\code{integer(1)}]\cr
#'   Number of individuals generated in each generation.
#' @param mating.pool.size [\code{integer(1)}]\cr
#'   Number of individuals which can potentially participate in the
#'   generation of offspring. Default is half of the population size.
#' @param representation [\code{character(1)}]\cr
#'   Genotype representation of the parameters. Available are binary, real, integer and
#'   permutation.
#' @param survival.strategy [\code{character(1)}]\cr
#'   Determines the survival strategy used by the EA. Possible are 'plus' for a classical
#'   (mu + lambda) strategy and 'comma' for (mu, lambda).
#' @param elite.size [\code{integer(1)}]\cr
#'   Number of fittest individuals of the current generation that shall be copied to the
#'   next generation without changing. Default is 0. Keep in mind, that the algorithm
#'   does not care about this option if the \code{survival.strategy} is set to 'plus'.
#' @param n.params [\code{integer(1)}]\cr
#'   Number of parameters of the objective function.
#' @param n.targets [\code{integer(1)}]\cr
#'   Number of target functions. Default is \code{1}. For bicriteria fitness functions
#'   this should therefore be set to \code{2}.
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximum number of generations. This is one possible stopping criterion.
#' @param termination.eps [\code{numeric(1)}]\cr
#'   The optimization process will stop if the gap between known optimum and current
#'   best individual falls below this threshold value.
#' @param show.info [\code{logical(1)}]\cr
#'   Logical flag indicating whether helpful information should be printed during the
#'   evolutionary process.
#' @param show.info.stepsize [\code{integer(1)}]\cr
#'   This positive value indicates after which iterations output shall be presented.
#' @param save.population.at [\code{integer}]\cr
#'   Which populations should be saved? Default is none.
#' @param mating.pool.generator [\code{function}]\cr
#'   Generator operator which implements a procedure to copy individuals from a
#'   given population to the mating pool, i. e., allow them to become parents.
#' @param generator [\code{ecr_generator}]\cr
#'   Generator operator of type \code{ecr_generator} for the generation of the initial
#'   population.
#' @param mutator [\code{ecr_mutator}]\cr
#'   Mutation operator of type \code{ecr_mutator}.
#' @param recombinator [\code{ecr_recombinator}]\cr
#'   Recombination operator of type \code{ecr_recombinator}.
#' @param mutator.control [\code{list}]\cr
#'   List of evolutionary parameters for the corresponding mutation operator. See the
#'   help pages for the mutation operators for the needed values.
#' @param recombinator.control [\code{list}]\cr
#'   List of evolutionary parameters for the corresponding recombination operator. See the
#'   help pages for the recombination operators for the needed values.
#' @param monitor [\code{function}]\cr
#'   Monitoring function. Default is \code{consoleMonitor}.
#' @return
#'   S3 object of type \code{ecr_control}.
#' @export
ecr.control = function(
  population.size,
  offspring.size,
  mating.pool.size = floor(population.size / 2),
  representation,
  survival.strategy = "plus",
  elite.size = 0L,
  n.params,
  n.targets = 1L,
  max.iter = 100L,
  termination.eps = 10^-1,
  show.info = TRUE,
  show.info.stepsize = 1L,
  save.population.at = integer(0),
  #FIXME: this should be of type 'ecr_operator' respectively 'ecr_generator'
  mating.pool.generator = parentSelection,
  generator = makeUniformGenerator(),
  mutator = list(gaussMutator),
  recombinator = intermediateRecombinator,
  mutator.control = list(),
  recombinator.control = list(),
  monitor = makeConsoleMonitor()) {
  checkArg(population.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(offspring.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  #FIXME: think about mating.pool.size
  mating.pool.size = convertInteger(mating.pool.size)
  checkArg(mating.pool.size, cl = "integer", len = 1L, lower = 2L, na.ok = FALSE)
  checkArg(representation, choices = getAvailableRepresentations())
  checkArg(survival.strategy, choices = c("plus", "comma"))
  checkArg(elite.size, cl = "integer", len = 1L, lower = 0L, na.ok = FALSE)
  checkArg(n.params, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(n.targets, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)

  checkArg(max.iter, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(termination.eps, cl = "numeric", len = 1L, lower = 0, na.ok = FALSE)

  checkArg(show.info, cl = "logical", len = 1L, na.ok = FALSE)
  checkArg(show.info.stepsize, cl = "integer", len = 1L, lower = 1, na.ok = FALSE)

  if (length(save.population.at) > 0) {
    checkArg(save.population.at, cl = "integer", lower = 0L, na.ok = FALSE)
  }

  checkArg(mutator, cl = "list", na.ok = FALSE)
  checkArg(mutator.control, cl = "list", na.ok = FALSE)
  checkArg(recombinator.control, cl = "list", na.ok = FALSE)
  if (!inherits(monitor, "ecr_monitor")) {
    stopf("Currently only monitor of type 'ecr_monitor' supported")
  }

  # Check arguments of mutator
  n.mutators = length(mutator)
  if (n.mutators == 0) {
    stopf("At least one mutator must be provided.")
  }

  mutator.control2 = vector(mode = "list", length = length(mutator))
  for (i in 1:n.mutators) {
    theMutator = mutator[[i]]
    if (!inherits(theMutator, "ecr_mutator")) {
      stopf("Mutator must be of class ecr_mutator, not %s", paste(attr(theMutator, "class")))
    }
    checkMutator(theMutator)
    mutator.control2[[i]] = prepareOperatorParameters(theMutator, mutator.control)
  }

  # Check arguments of recombinator
  if (!inherits(recombinator, "ecr_recombinator")) {
    stopf("Recombinator must be of class ecr_recombinator, not %s", paste(attr(mutator, "class")))
  }
  checkRecombinator(recombinator)
  recombinator.control = prepareOperatorParameters(recombinator, recombinator.control)

  if (!inherits(generator, "ecr_generator")) {
    stopf("Generator must be of class ecr_generator, not %s", paste(attr(generator, "class")))
  }

  sapply(c(generator, mutator, recombinator), function(operator) {
    if (!is.supported(operator, representation)) {
      stop(paste("Mutator '", getOperatorName(operator), "' is not compatible with representation '", representation, "'!", sep = ""))
    }
  })

  # If the survival strategy is (mu + lambda), than the number of generated offspring in each iteration
  # must greater or equal to the population size
  if (survival.strategy == "comma" && offspring.size < population.size) {
    stopf("The (mu, lambda) survival strategy requires the number of generated offspring in each generation
      to be greater or equal to the population size, but %i < %i", offspring.size, population.size)
  }

  if (survival.strategy == "comma" && elite.size >= population.size) {
     stopf("Elite.size must be smaller than population.size! Otherwise each population would be the same.")
  }

  structure(list(
    population.size = population.size,
    offspring.size = offspring.size,
    mating.pool.size = mating.pool.size,
    representation = representation,
    survival.strategy = survival.strategy,
    elite.size = elite.size,
    n.params = n.params,
    n.targets = n.targets,
    max.iter = max.iter,
    termination.eps = termination.eps,
    mating.pool.generator = mating.pool.generator,
    generator = generator,
    mutator = mutator,
    n.mutators = n.mutators,
    recombinator = recombinator,
    mutator.control = mutator.control2,
    recombinator.control = recombinator.control,
    show.info = show.info,
    show.info.stepsize = show.info.stepsize,
    save.population.at = save.population.at,
    monitor = monitor),
  class = "ecr_control")
}

# Helper function which constructs control object for a given operator
# and checks the user parameters for validity.
#
# @param operator [\code{ecr_operator}]\cr
#   Operator object.
# @param parameters [\code{list}]\cr
#   List of parameters provedided by the user for the operator.
# @return [\code{list}]
#   List of checked parameters.
prepareOperatorParameters = function(operator, input.params) {
  defaults = getOperatorDefaultParameters(operator)
  params = insert(defaults, input.params)
  params[setdiff(names(params), names(defaults))] = NULL
  do.call(getOperatorCheckFunction(operator), list(params))
  return(params)
}

#' Print ecr control object.
#'
#' @param x [\code{ecr_control}]\cr
#'   Control object.
#' @param ... [any]\cr
#'   Not used.
#'
#' @S3method print ecr_control
print.ecr_control = function(x, ...) {
  catf("[ecr CONTROL OBJECT]\n")

  catf("Objective function:")
  if (x$n.targets == 1L) {
    catf("Optimizing mono-criteria objective function.")
  } else {
    catf("Optimizing multi-criteria objective function (%i targets).", x$n.targets)
  }
  catf("Number of parameters         : %i", x$n.params)
  if (x$n.targets > 1L) {
    catf("Number of targets            : %i", x$n.targets)
  }
  catf("")

  catf("Evolutionary parameters:")
  catf("Population size              : %i", x$population.size)
  catf("Offspring size               : %i", x$offspring.size)
  catf("Mating pool size             : %i", x$mating.pool.size)
  catf("Representation               : %s", x$representation)
  catf("Survival strategy            : %s", if (x$survival.strategy == "plus") "(mu + lambda)" else "(mu, lambda)")
  if (x$elite.size > 0L && x$survival.strategy == "comma") {
    catf("(Using elitism with elite count %i, i.e., %.2g%% of the fittest candidates in each generation will survive)", x$elite.size, as.numeric(x$elite.size)/x$population.size)
  }

  catf("")
  catf("Evolutionary operators:")
  catf("Generator object             : %s", getOperatorName(x$generator))
  catf("Mutation operators           : ")
  # print(x$mutator)
  # for (mutator in x$mutator[[1]]) {
  #   catf("  %s (%s)", getOperatorName(x$mutator), getParametersAsString(x$mutator.control))
  # }
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
