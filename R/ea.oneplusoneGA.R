#' @title
#' Simple (1 + 1) Genetic Algorithm.
#'
#' @description
#' The simplest evolutionary algorithm one can imagine, namely the (1+1) EA/GA.
#' Maintains a population of a single individual x and uses just bitplip mutation
#' to generate a child y (obviously no recombination takes place), i.e., each gene
#' of x is flipped with probability \code{p} independently. The best individual
#' survives.
#' This algorithm is of particular interest in the theory of evolutionary algorithms
#' and its performance is well understood for different function families.
#' A lot of interesting results exist.
#'
#' @note This helper function hides the regular \pkg{ecr} interface and offers a more
#' R like interface to a simple evolutionary algorithm which works on binary valued
#' vectors.
#'
#' @keywords optimize
#'
#' @template arg_optimization_task
#' @param p [\code{numeric(1)}]\cr
#'   Mutation probability for bitplip mutation.
#'   Default is \eqn{1/n} where n is the length of the gene.
#' @template arg_max_iter
#' @template arg_max_evals
#' @template arg_max_time
#' @param ... [any]\cr
#'   Further arguments passed to \code{\link{setupECRControl}}.
#' @return [\code{ecr_single_objective_result}]
#' @export
onePlusOneGA = function(
  task,
  p = NULL,
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
    stopf("(1+1)-GA works for numeric functions only.")
  }

  if (!is.null(p)) {
    assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
  }

  # get lengths of genome to determine best theoretical mutation probability 1/n
  par.set = task$par.set
  n = getParamLengths(par.set)

  # control object
  ctrl = setupECRControl(
    n.population = 1L,
    n.mating.pool = 1L,
    n.offspring = 1L,
    survival.strategy = "plus",
    representation = "binary",
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
    parent.selector = setupSimpleSelector(),
    recombinator = setupNullRecombinator(), # no recombination at all
    mutator = setupBitFlipMutator(p = coalesce(p, 1.0 / n)),
    survival.selector = setupGreedySelector()
  )

  return(doTheEvolution(task, ctrl))
}
