#' @title
#' Implementation of the NSGA-II EMOA algorithm by Deb.
#'
#' @description
#' The AS-EMOA, short for aspiration set evolutionary multi-objective
#' algorithm aims to incorporate expert knowledge into multi-objective optimization [1].
#' The algorithm expects an aspiration set, i.e., a set of reference points. It
#' then creates an appriximation of the pareto front close to the aspiration set
#' utilizing the average Hausdorff distance.
#'
#' @note
#' This is a pure R implementation of the AS-EMOA algorithm. It hides the regular
#' \pkg{ecr} interface and offers a more R like interface while still being quite
#' adaptable.
#'
#' @keywords optimize
#'
#' @references
#'   Rudolph, G., Schuetze, S., Grimme, C., Trautmann, H: An Aspiration Set
#'   EMOA Based on Averaged Hausdorff Distances. LION 2014: 153-156.
#'
#' @template arg_optimization_task
#' @param n.population [\code{integer(1)}]\cr
#'   Population size. Default is \code{100}.
#' @param aspiration.set [\code{matrix}]\cr
#'   The aspiration set. Each column contains one point of the set.
#' @param n.archive [\code{integer(1)}]\cr
#'   Size of the pareto archive, i.e., the number of nondominated points which we
#'   aim to generate. Default is \code{ncol(aspiration.set)}.
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
asemoa = function(
  task,
  n.population = 100L,
  aspiration.set = NULL,
  n.archive,
  parent.selector = setupSimpleSelector(),
  mutator = setupPolynomialMutator(eta = 25, p = 0.2),
  recombinator = setupSBXRecombinator(eta = 15, p = 0.7),
  max.iter = 100L,
  max.evals = NULL,
  max.time = NULL,
  ...) {

  if (isSmoofFunction(task)) {
    task = makeOptimizationTask(task)
  }
  assertMatrix(aspiration.set, mode = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 2L)
  if (nrow(aspiration.set) != task$n.objectives) {
    stopf("AS-EMAO: Dimension of the aspiration set needs to be equal to the number of objectives,
      but %i <> %i.", nrow(aspiration.set), task$n.objectives)
  }
  if (is.null(n.archive)) {
    n.archive = ncol(aspiration.set)
  }
  assertInt(n.archive, na.ok = FALSE, lower = 2L)

  # This is the main selection mechanism of the AS-EMOA.
  # Remove the point which leads to highest
  deltaOneUpdate = function(set, aspiration.set) {
    # here we need to apply this strange information. See the reference for details
    # yeah, I could use range here but it is more readable this way
    min1 = min(aspiration.set[1L, ])
    min2 = min(aspiration.set[2L, ])
    max1 = max(aspiration.set[1L, ])
    max2 = max(aspiration.set[2L, ])

    # transform
    set[1L, ] = (set[1L, ] - min1) / (max2 - min2)
    set[2L, ] = (set[2L, ] - min2) / (max1 - min1)

    return(computeAverageHausdorffDistance(set, aspiration.set))
  }

  # Implementation of surival selection operator of the AS-EMOA algorithm.
  asemoaSelector = makeSelector(
    selector = function(fitness, n.select, task, control, opt.state) {
      all.idx = 1:ncol(fitness)

      # filter nondominated points
      nondom.idx = which.nondominated(fitness)
      pop.idx = all.idx[nondom.idx]
      fitness = fitness[, nondom.idx, drop = FALSE]

      n.archive = control$n.archive
      # if maximal number of individuals is not exceeded yet
      # simply return
      if (length(pop.idx) <= n.archive) {
        return(pop.idx)
      }

      # Otherwise we need to do the computationally more expensive part
      hausdorffDistances = lapply(all.idx, function(idx) {
        deltaOneUpdate(fitness[, -idx, drop = FALSE], control$aspiration.set)
      })

      #FIXME: here we need to check if there are multiple elements with this distance
      tmp = getMinIndex(hausdorffDistances)
      return(setdiff(all.idx, tmp))
    },
    supported.objectives = "multi-objective",
    name = "AS-EMOA selector",
    description = "Selection takes place based on (modified) average Hausdorff metric"
  )

  asemoaGenerator = makeGenerator(
    generator = function(size, task, control) {
      uniformGenerator = setupUniformGenerator()
      population = uniformGenerator(size, task, control)
      #NOTE: here we use the objective function to compute the fitness values
      fitness = evaluateFitness(population, task$fitness.fun, task, control)
      # now filter out dominated solutions
      nondom.idx = which.nondominated(fitness)
      population$individuals = population$individuals[nondom.idx]
      return(population)
    },
    name = "AS-EMOA generator",
    description = "Generates uniformaly and reduces to non-dominated set",
    supported = "float"
  )

  # AS-EMOA control object
  ctrl = setupECRControl(
    n.population = n.population,
    n.offspring = 1L,
    representation = "float",
    stopping.conditions = list(
      setupMaximumEvaluationsTerminator(max.evals),
      setupMaximumTimeTerminator(max.time),
      setupMaximumIterationsTerminator(max.iter)
    ),
    ...
  )

  ctrl = setupEvolutionaryOperators(
    ctrl,
    parent.selector = parent.selector,
    recombinator = recombinator,
    generator = asemoaGenerator,
    mutator = mutator,
    survival.selector = asemoaSelector
  )

  #FIXME: this is rather ugly. We simply add some more args to the control object
  # without sanity checks and stuff like that.
  ctrl$n.archive = n.archive
  ctrl$aspiration.set = aspiration.set

  return(doTheEvolution(task, ctrl))
}
