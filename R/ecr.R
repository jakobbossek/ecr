#' Working horse of the ecr package.
#'
#' Takes a function and searches for global optimum with an evolutionary approach.
#'
#' @param objective.fun [\code{function}]\cr
#'   Target function.
#' @param control [\code{ecr.control}]\cr
#'   Control object.
#' @param global.optimum [\code{numeric}]\cr
#'   Parameter combination of the global optimum of the fun. This parameter is optional.
#' @param lower [\code{numeric}]\cr
#'   Lower box constraints for the parameters. These are needed, if representation type is set
#'   to 'float'.
#' @param upper [\code{numeric}]\cr
#'   Upper box constraints for the parameters. These are needed, if representation type is set
#'   to 'float'.
#' return [\code{ecrResult}]
#'   Object of type \code{ecrResult} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{trace \code{ecrTrace}}{Optimization path.}
#'   }
#' @export
ecr = function(objective.fun, control, global.optimum = NA, lower = NA, upper = NA) {
  n.params = control$n.params
  max.iter = control$max.iter
  population.size = control$population.size
  mating.pool.size = control$mating.pool.size
  offspring.size = control$offspring.size
  show.info = control$show.info
  show.info.stepsize = control$show.info.stepsize
  termination.eps = control$termination.eps
  monitor = control$monitor

  #FIXME: maybe better outsource the sanity checks to dedicated function
  if (!any(is.na(global.optimum))) {
    if (length(global.optimum) != control$n.params) {
      stopf("Given global optimum %s suggests %i parameters, but objective function has %i parameters.",
        paste("(", strImplode(global.optimum, sep = ","), ")", sep=""), length(global.optimum), control$n.params)
    }
  }

  if (is.na(lower) && is.na(upper) && is_soo_function(objective.fun)) {
    lower = lower_bounds(objective.fun)
    upper = upper_bounds(objective.fun)
  }

  if ((is.na(lower) || is.na(upper)) && control$representation %in% c("float")) {
    stopf("Lower and upper box constraints needed for representation type 'float'.")
  }

  populationGenerator = control$generator
  matingPoolGenerator = control$mating.pool.generator

  population = populationGenerator(population.size, n.params, lower, upper, control)
  population = computeFitness(population, objective.fun)
  best = getBestIndividual(population)
  trace = makeTrace(n.params)
  trace = addToTrace(trace, best, 0)

  i = 1L
  while (!isTerminiationCriterionFullfilled(i, max.iter, global.optimum, best, termination.eps)) {
    if (show.info && (i %% show.info.stepsize == 0L)) {
      monitor(objective.fun, population, trace, i, control)
    }
    parents = matingPoolGenerator(population, mating.pool.size)

    offspring = generateOffspring(parents, objective.fun, control)

    population = selectForSurvival(
      population,
      offspring,
      population.size,
      strategy = control$survival.strategy,
      elite.size = control$elite.size)

    best = getBestIndividual(population)

    trace = addToTrace(trace, best, i)

    i = i + 1
  }

  if (show.info) {
    catf("\nEA terminated.")
  }

  return(
    structure(list(
      best.param = best$individual,
      best.value = best$fitness,
      trace = trace
      ), class = "ecr_result")
  )
}
