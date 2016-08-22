#' @title
#' Implementation of the NSGA-II EMOA algorithm by Deb.
#'
#' @description
#' The AS-EMOA, short for aspiration set evolutionary multi-objective
#' algorithm aims to incorporate expert knowledge into multi-objective optimization [1].
#' The algorithm expects an aspiration set, i.e., a set of reference points. It
#' then creates an approximation of the pareto front close to the aspiration set
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
#' [1] Rudolph, G., Schuetze, S., Grimme, C., Trautmann, H: An Aspiration Set
#' EMOA Based on Averaged Hausdorff Distances. LION 2014: 153-156.
#' [2] G. Rudolph, O. Schuetze, C. Grimme, and H. Trautmann: A Multiobjective
#' Evolutionary Algorithm Guided by Averaged Hausdorff Distance to Aspiration
#' Sets, pp. 261-273 in A.-A. Tantar et al. (eds.): Proceedings of EVOLVE - A
#' bridge between Probability, Set Oriented Numerics and Evolutionary Computation
#' V, Springer: Berlin Heidelberg 2014.
#'
#' @template arg_optimization_task
#' @param n.population [\code{integer(1)}]\cr
#'   Population size. Default is \code{10}.
#' @param aspiration.set [\code{matrix}]\cr
#'   The aspiration set. Each column contains one point of the set.
#' @param normalize.fun [\code{function}]\cr
#'   Function used to normalize fitness values of the individuals
#'   before computation of the average Hausdorff distance.
#'   The function must have the formal arguments \dQuote{set} and \dQuote{aspiration.set}.
#'   Default is \code{NULL}, i.e., no normalization at all.
#' @param dist.fun [\code{function}]\cr
#'   Distance function used internally by Hausdorff metric to compute distance
#'   between two points. Expects a single vector of coordinate-wise differences
#'   between points.
#'   Default is \code{computeEuclideanDistance}.
#' @param p [\code{numeric(1)}]\cr
#'   Parameter \eqn{p} for the average Hausdorff metric. Default is 1.
#' @template arg_parent_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_max_iter
#' @template arg_max_evals
#' @template arg_max_time
#' @param ... [any]\cr
#'   Further arguments passed to \code{\link{setupECRControl}}.
#' @return [\code{ecr_asemoa_result, ecr_multi_objective_result}]
#' @example examples/ex_asemoa.R
#' @export
asemoa = function(
  task,
  n.population = 10L,
  aspiration.set = NULL,
  normalize.fun = NULL,
  dist.fun = ecr:::computeEuclideanDistance,
  p = 1,
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
  n.archive = ncol(aspiration.set)

  assertInt(n.population, lower = 5L)
  assertInt(n.archive, lower = 3L)
  if (!is.null(normalize.fun)) {
    assertFunction(normalize.fun, args = c("set", "aspiration.set"), ordered = TRUE)
  }
  assertFunction(dist.fun)
  assertNumber(p, lower = 0.001)

  # This is the main selection mechanism of the AS-EMOA.
  # Remove the point which leads to highest
  deltaOneUpdate = function(set, aspiration.set) {
    if (!is.null(normalize.fun)) {
      set = normalize.fun(set, aspiration.set)
    }
    return(computeAverageHausdorffDistance(set, aspiration.set, p = p, dist.fun = dist.fun))
  }

  fastASEMOASelector = makeSelector(
    selector = function(fitness, n.select, task, control, opt.state) {
      aspiration.set = control$aspiration.set
      n.archive = ncol(aspiration.set)

      # get nondominated points
      nondom.idx = which.nondominated(fitness)
      fitness.pop = fitness[, nondom.idx, drop = FALSE]
      if (!is.null(normalize.fun)) {
        fitness.pop = normalize.fun(fitness.pop, aspiration.set)
      }
      n.pop = length(nondom.idx)

      # archive size exceeded! We need to drop one individual from the population
      if (n.pop <= n.archive) {
        return(nondom.idx)
      }

      GDp = 0
      IGDp = 0

      GDps = numeric(n.pop)
      IGDps = numeric(n.pop)

      # initialize for later use
      IGDp1s = rep(0.0, n.pop)
      IGDp2s = rep(0.0, n.pop)

      for (i in seq_len(n.pop)) {
        # GP_p contribution of archive point a
        GDps[i] = computeDistanceFromPointToSetOfPoints(fitness.pop[, i], aspiration.set)
        # add GD_p contribution of a
        GDp = GDp + GDps[i]
      }

      for (i in seq_len(n.archive)) {
        r = aspiration.set[, i]
        # if (!is.null(normalize.fun)) {
        #   fitness.pop2 = normalize.fun(fitness.pop, aspiration.set)
        # }
        rdists = computeDistancesFromPointToSetOfPoints(r, fitness.pop)
        astar.idx = which.min(rdists)
        d1 = rdists[astar.idx] # distance to closest population point
        d2 = min(rdists[-astar.idx]) # distance to 2nd closest population point
        IGDp = IGDp + d1 # add IGD_p contribution of r
        IGDp1s[astar.idx] = IGDp1s[astar.idx] + d1 # sum IGD_p contributions with a* involved
        IGDp2s[astar.idx] = IGDp2s[astar.idx] + d2 # sum IGD_p contributions without a*
      }
      dpmin = Inf
      gdpmin = Inf
      for (i in seq_len(n.pop)) {
        gdp = GDp - GDps[i] # value of GD_p if a deleted
        igdp = IGDp - IGDp1s[i] + IGDp2s[i] # value of IGD_p if a deleted
        dp = max(gdp / (n.pop - 1), igdp / n.archive) # delta_1 if a deleted
        if (dp < dpmin | (dp == dpmin & gdp < gdpmin)) {
          dpmin = dp # store smallest delta_1 seen so far
          gdpmin = gdp # store smallest gdp since last improvement
          astar.idx = i
        }
      }
      return(nondom.idx[-astar.idx])
    },
    supported.objectives = "multi-objective",
    name = "Fast AS-EMOA selector",
    description = "Uses sped up delta-p update."
  )

  # Implementation of surival selection operator of the AS-EMOA algorithm.
  asemoaSelector = makeSelector(
    selector = function(fitness, n.select, task, control, opt.state) {
      aspiration.set = control$aspiration.set
      n.archive = ncol(aspiration.set)

      # get offspring
      all.idx = 1:ncol(fitness)

      # filter nondominated points
      nondom.idx = which.nondominated(fitness)
      pop.idx = all.idx[nondom.idx]
      fitness = fitness[, nondom.idx, drop = FALSE]

      # if maximal number of individuals is not exceeded yet
      # simply return
      if (length(pop.idx) <= n.archive) {
        return(pop.idx)
      }

      # Otherwise we need to do the computationally more expensive part
      has = lapply(pop.idx, function(idx) {
        deltaOneUpdate(fitness[, -idx, drop = FALSE], aspiration.set)
      })

      # set of elements whose deletion from archive leads to
      # highest decrese of delta_p
      astar = which.min(has)

      # if there are multiple points, choose the one which leads
      # to minimal generational distance if removed
      if (length(astar) > 1L) {
        generationalDistances = lapply(astar, function(idx) {
          computeGenerationalDistance(fitness[, -idx, drop = FALSE], aspiration.set, p = p, dist.fun = dist.fun)
        })
        # determine which is minimal
        min.idx = getMinIndex(generationalDistances)
        astar = astar[min.idx]
      } else {
        astar = getMinIndex(has)
      }

      return(setdiff(pop.idx, astar))
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
    survival.strategy = "plus",
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
    survival.selector = fastASEMOASelector
  )

  #FIXME: this is rather ugly. We simply add some more args to the control object
  # without sanity checks and stuff like that.
  ctrl$aspiration.set = aspiration.set

  res = doTheEvolution(task, ctrl)
  res = BBmisc::addClasses(res, "ecr_asemoa_result")
  return(res)
}

# @title
# Normalization function introduced in [1].
#
# @param set [\code{matrix}]\cr
#   Fitness values of the candidate solutions.
# @param aspiration.set [\code{matrix}]\cr
#   Aspiration set.
# @return [\code{matrix}]
asemoaNormalize1 = function(set, aspiration.set) {
  min1 = min(aspiration.set[1L, ])
  min2 = min(aspiration.set[2L, ])
  max1 = max(aspiration.set[1L, ])
  max2 = max(aspiration.set[2L, ])

  # transform
  set[1L, ] = (set[1L, ] - min1) / (max2 - min2)
  set[2L, ] = (set[2L, ] - min2) / (max1 - min1)
  return(set)
}
