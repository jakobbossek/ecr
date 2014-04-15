#' Generates control object.
#'
#' The esoo package offers a framework for evolutionary computing and therefore offers
#' a lot of customization options. The control object is a simple but powerful
#' wrapper for all these options and sets convenient default options.
#'
#' @param population.size [\code{integer(1)}]\cr
#'   Number of individuals in the population.
#' @param offspring.size [\code{integer(1)}]\cr
#'   Number of idividuals generated in each generation.
#' @param representation [\code{character(1)}]\cr
#'   Genotype representation of the parameters. Available are binary, real, integer and
#'   permutation.
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
#' @param generator [\code{esoo_generator}]\cr
#'   Generator operator of type \code{esoo_generator} for the generation of the initial
#'   population.
#' @param mutator [\code{esoo_mutator}]\cr
#'   Mutation operator of type \code{esoo_mutator}.
#' @param recombinator [\code{esoo_recombinator}]\cr
#'   Recombination operator of type \code{esoo_recombinator}.
#' @param mutator.gauss.prob [\code{numeric(1)}]\cr
#'   Probability of mutation for the gauss mutation operator.
#' @param mutator.gauss.sd [\code{numeric(1)}]\cr
#'   Standard deviance of the Gauss mutation, i. e., the mutation strength.
#' @return
#'   S3 object of type \code{esoo.control}.
#' @export
esoo.control = function(
  population.size,
  offspring.size,
  representation,
  n.params,
  n.targets = 1L,
  max.iter = 100L,
  termination.eps = 10^-1,
  show.info = TRUE,
  show.info.stepsize = 1L,
  generator = makeUniformGenerator(),
  mutator = makeGaussMutator(),
  recombinator = makeIntermediateRecombinator(),
  mutator.gauss.prob = 1,
  mutator.gauss.sd = 0.05) {
  checkArg(population.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(offspring.size, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(representation, choices = getAvailableRepresentations())
  checkArg(n.params, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(n.targets, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)

  checkArg(max.iter, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  checkArg(termination.eps, cl = "numeric", len = 1L, lower = 0, na.ok = FALSE)

  checkArg(show.info, cl = "logical", len = 1L, na.ok = FALSE)
  checkArg(show.info.stepsize, cl = "integer", len = 1L, lower = 1, na.ok = FALSE)
  checkArg(mutator.gauss.prob, cl = "numeric", len = 1L, lower = 0, upper = 1, na.ok = FALSE)
  checkArg(mutator.gauss.sd, cl = "numeric", len = 1L, lower = 0.0001, na.ok = FALSE)
  if (!inherits(mutator, "esoo_mutator")) {
    stopf("Mutator must be of class esoo_mutator, not %s", paste(attr(mutator, "class")))
  }
  if (!inherits(generator, "esoo_generator")) {
    stopf("Generator must be of class esoo_generatorm, not %s", paste(attr(generator, "class")))
  }
  if (!inherits(recombinator, "esoo_recombinator")) {
    stopf("Recombinator must be of class esoo_recombinator, not %s", paste(attr(mutator, "class")))
  }
  if (!is.supported(mutator, representation)) {
    stop(paste("Mutator'", getOperatorName(mutator), "' is not compatible with representation '", representation, "'!"))
  }

  structure(list(
    population.size = population.size,
    offspring.size = offspring.size,
    representation = representation,
    n.params = n.params,
    n.targets = n.targets,
    max.iter = max.iter,
    termination.eps = termination.eps,
    generator = generator,
    mutator = mutator,
    recombinator = recombinator,
    mutator.gauss.prob = mutator.gauss.prob,
    mutator.gauss.sd = mutator.gauss.sd,
    show.info = show.info,
    show.info.stepsize = show.info.stepsize), class = "esoo_control")
}

#' Print esoo control object.
#'
#' @param x [\code{esoo_control}]\cr
#'   Control object.
#' @param ... [any]\cr
#'   Not used.
#'
#' @method print esoo_control
print.esoo_control = function(x, ...) {
  catf("[ESOO CONTROL OBJECT]\n")

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
  catf("Representation               : %s", x$representation)

  catf("")
  catf("Evolutionary operators:")
  catf("Mutation operator:           : %s", getOperatorName(x$mutator))
  catf("Recombination operator:      : %s", getOperatorName(x$recombinator))
  #FIXME: need to print the correct operator params without sick if-else messup
}
