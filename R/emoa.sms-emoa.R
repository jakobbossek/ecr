#' @title
#'   Implementation of the SMS-EMOA by Emmerich et al.
#'
#' @description
#'   Pure R implementation of the SMS-EMOA. This algorithm belongs to the group
#'   of indicator based multi-objective evolutionary algorithms. In each generation,
#'   the SMS-EMOA selects two parents uniformly at, applies recombination and mutation
#'   and finally selects the best subset of individuals among all subsets by maximizing
#'   the Hypervolume indicator.
#'
#' @note
#'   This helper function hides the regular \pkg{ecr} interface and offers a more
#'   R like interface of this state of the art EMOA.
#'
#' @keywords optimize
#'
#' @template arg_optimization_task
#' @param n.population [\code{integer(1)}]\cr
#'   Population size. Default is \code{100}.
#' @param ref.point [\code{numeric}]\cr
#'   Reference point for the hypervolume computation. Default is (11, ..., 11)'
#'   with the corresponding dimension.
#' @template arg_parent_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_max_iter
#' @template arg_max_evals
#' @template arg_max_time
#' @param ... [any]\cr
#'   Further arguments passed to \code{\link{setupECRControl}}.
#' @return [\code{ecr_ecr_multi_objective_result}]
#' @export
smsemoa = function(
  task,
  n.population = 100L,
  ref.point = NULL,
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  max.iter = NULL,
  max.evals = NULL,
  max.time = NULL, ...) {

  hypervolumeSelector = makeSelector(
    selector = function(population, storage, task, n.select, control) {
      fitness = population$fitness
      population = population$individuals

      # do non-dominated sorting
      nds.res = doNondominatedSorting(fitness)
      ranks = nds.res$ranks
      idx.max = which(ranks == max(ranks))

      # there is exactly one individual that is "maximally" dominated
      if (length(idx.max) == 1L) {
        return(makePopulation(population[-idx.max], fitness[, -idx.max, drop = FALSE]))
      }

      # compute exclusive hypervolume contributions and remove the one with the smallest
      hvctrbs = computeHypervolumeContribution(fitness[, idx.max, drop = FALSE], ref.point = control$ref.point)
      die.idx = idx.max[getMinIndex(hvctrbs)]

      return(makePopulation(population[-die.idx], fitness[, -die.idx, drop = FALSE]))
    },
    supported.objectives = "multi-objective",
    name = "Hypervolume contribution selector",
    description = "description"
  )

  if (isSmoofFunction(task)) {
    task = makeOptimizationTask(task)
  }
  assertClass(task, "ecr_optimization_task")

  # SMS-EMOA control object
  ctrl = setupECRControl(
    n.population = n.population,
    n.offspring = 1L,
    representation = "float",
    stopping.conditions = list(
      makeMaximumEvaluationsStoppingCondition(max.evals),
      makeMaximumTimeStoppingCondition(max.time),
      makeMaximumIterationsStoppingCondition(max.iter)
    ),
    ...
  )
  ctrl = setupEvolutionaryOperators(
    ctrl,
    parent.selector = parent.selector,
    recombinator = recombinator,
    mutator = mutator,
    survival.selector = hypervolumeSelector
  )

  ctrl$ref.point = ref.point
  if (is.null(ref.point)) {
    ctrl$ref.point = rep(11, task$n.objectives)
  }
  if (length(ctrl$ref.point) != task$n.objectives) {
    stopf("Reference point ref.point needs to have as many components as objectives.")
  }

  return(doTheEvolution(task, ctrl))
}
