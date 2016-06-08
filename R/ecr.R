#' @title
#' Interface to \pkg{ecr} similar to the \code{\link[stats]{optim}} function.
#'
#' @description
#' The most flexible way to setup evolutionary algorithms with \pkg{ecr} is by
#' explicitely generating a task and a control object and passing both to
#' \code{\link{doTheEvolution}}. Although this approach is highly flexible
#' and very readable it requires quite a lot of code. However, in everyday
#' life R users frequently need to optimize a single-objective R function.
#' The \code{ecr} function thus provides a more R like interface for single
#' objective optimization similar to the interface of the \code{\link[stats]{optim}}
#' function.
#'
#' @note
#' This helper function is applicable for single-objective optimization based
#' on default encodings, i.e., binary, float and permutation, only.
#' If your function at hand has multiple objectives or you need special
#' encodings and operators you need to work with \code{\link{doTheEvolution}}
#' directly.
#'
#' @keywords optimize
#'
#' @seealso \code{\link{setupECRControl}} for building the control object,
#' \code{\link{makeOptimizationTask}} to define an optimization problem and
#' \code{\link{doTheEvolution}} for the main working horse of \pkg{ecr}.
#'
#' @param obj.fun [\code{function}]\cr
#'   The single-objective target function. Can be any R function which takes a
#'   single vector as input and returns a scalar value describing the vectors
#'   fitness.
#' @param n.dim [\code{integer(1)}]\cr
#'   Dimension of the decision space.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space in case
#'   of float or permutation encoding.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space in case
#'   of float or permutation encoding.
#' @param n.bits [\code{integer(1)}]\cr
#'   Number of bits to use for binary representation.
#' @template arg_representation
#' @template arg_n_population
#' @template arg_n_offspring
#' @template arg_n_mating_pool
#' @template arg_survival_strategy
#' @template arg_n_elite
#' @template arg_vectorized_evaluation
#' @template arg_custom_constants
#' @template arg_logger
#' @template arg_monitor
#' @template arg_max_iter
#' @template arg_max_evals
#' @template arg_max_time
#' @template arg_more_args
#' @template arg_initial_population
#' @template arg_parent_selector
#' @template arg_survival_selector
#' @template arg_generator
#' @template arg_mutator
#' @template arg_recombinator
#' @return [\code{\link{ecr_result}}]
#' @examples
#' fn = function(x) {
#'    sum(x^2)
#'  }
#'
#' res = ecr(fn, n.dim = 2L, lower = c(-5, -5), upper = c(5, 5),
#'  representation = "float", n.population = 20L, n.offspring = 10L, max.iter = 30L)
#' @export
ecr = function(
  obj.fun, n.dim, lower = NULL, upper = NULL, n.bits,
  representation, n.population, n.offspring, n.mating.pool = floor(n.population / 2),
  survival.strategy = "plus", n.elite = 0L, vectorized.evaluation = FALSE,
  custom.constants = list(), logger = NULL, monitor = setupConsoleMonitor(),
  max.iter = 100L, max.evals = NULL, max.time = NULL,
  more.args = list(), initial.population = NULL,
  parent.selector = getDefaultEvolutionaryOperators(representation, "parent.selector"),
  survival.selector = getDefaultEvolutionaryOperators(representation, "survival.selector"),
  generator = getDefaultEvolutionaryOperators(representation, "generator"),
  mutator = getDefaultEvolutionaryOperators(representation, "mutator"),
  recombinator = getDefaultEvolutionaryOperators(representation, "recombinator")) {

  # simply pass stuff down to control object constructor
  control = setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    n.mating.pool = n.mating.pool,
    n.elite = n.elite,
    survival.strategy = survival.strategy,
    representation = representation,
    custom.constants = custom.constants,
    logger = logger,
    monitor = monitor,
    vectorized.evaluation = vectorized.evaluation,
    list(
      setupMaximumIterationsTerminator(max.iter = max.iter),
      setupMaximumEvaluationsTerminator(max.evals),
      setupMaximumTimeTerminator(max.time)
    )
  )

  control = setupEvolutionaryOperators(
    control,
    parent.selector = parent.selector,
    survival.selector = survival.selector,
    generator = generator,
    mutator = mutator,
    recombinator = recombinator
  )

  # now build the task
  # We distinguish in binary, float and permutation here
  par.set = if (representation == "float") {
    if (length(lower) != n.dim | length(upper) != n.dim) {
      stopf("Lower and upper bounds need to be of length %i!", n.dim)
    }
    makeNumericParamSet("x", lower = lower, upper = upper, len = n.dim)
  } else if (representation == "binary") {
    makeParamSet(makeIntegerVectorParam("x", lower = 0, upper = 1, len = n.dim))
  } else if (representation == "permutation") {
    makeNumericParamSet("x", lower = 1, upper = n.dim, len = n.dim)
  }
  smoof.fun = makeSingleObjectiveFunction(fn = obj.fun, par.set = par.set, name = "ECR")

  return(doTheEvolution(smoof.fun, control, initial.population, more.args))
}
