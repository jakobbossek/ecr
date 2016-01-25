#' @title
#' Simple (mu + lambda) EA implementation.
#'
#' @description
#' A simple evolutionary (mu + lambda) strategy for the optimization
#' of real-valued functions.
#'
#' @note This helper function hides the regular \pkg{ecr} interface and offers a more
#' R like interface to a simple evolutionary algorithm which works on real valued
#' vectors.
#'
#' @keywords optimize
#'
#' @template arg_optimization_task
#' @param n.population [\code{integer(1)}]\cr
#'   Population size (mu).
#'   Default is 10.
#' @param n.offspring [\code{integer(1)}]\cr
#'   Number of offspring (lambda) generated in each generation.
#'   Default is 10.
#' @template arg_parent_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_survival_selector
#' @template arg_max_iter
#' @template arg_max_evals
#' @template arg_max_time
#' @param ... [any]\cr
#'   Further arguments passed to \code{\link{setupECRControl}}.
#' @return [\code{ecr_single_objective_result}]
#' @export
simpleEA = function(
  task,
  n.population = 10L,
  n.offspring = 10L,
  parent.selector = setupSimpleSelector(),
  mutator = setupGaussMutator(),
  recombinator = setupCrossoverRecombinator(),
  survival.selector = setupTournamentSelector(2L),
  max.iter = NULL,
  max.evals = NULL,
  max.time = NULL, ...) {

  if (isSmoofFunction(task)) {
    task = makeOptimizationTask(task)
  }
  assertClass(task, "ecr_optimization_task")
  if (!isSmoofFunction(task$fitness.fun)) {
    stopf("Objective fun needs to be of type smoof_function.")
  }
  if (!isNumeric(task$par.set, include.int = FALSE)) {
    stopf("(mu + lambda)-EA works for real-valued functions only.")
  }

  # control object
  ctrl = setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    survival.strategy = "plus",
    representation = "float",
    stopping.conditions = list(
      setupMaximumEvaluationsTerminator(max.evals),
      setupMaximumTimeTerminator(max.time),
      setupMaximumIterationsTerminator(max.iter)
    ),
    ...
  )

  # operator setup
  ctrl = setupEvolutionaryOperators(
    ctrl,
    parent.selector = parent.selector,
    recombinator = recombinator,
    mutator = mutator,
    survival.selector = survival.selector
  )

  return(doTheEvolution(task, ctrl))
}
