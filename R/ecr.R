#' Working horse of the ecr package.
#'
#' Takes a function and searches for a global optimum with an evolutionary approach.
#'
#' @param objective.fun [\code{smoof_function}]\cr
#'   Single objective target function of type \code{smoof_function}.
#' @param control [\code{ecr.control}]\cr
#'   Control object.
#' @return [\code{ecrResult}]
#'   Object of type \code{ecr_result} containing a list:
#'   \itemize{
#'    \item{objective.fun \code{smoof_function}}{Objective function.}
#'    \item{control \code{ect_control}}{Control object.}
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{opt.path \code{optPath}}{Optimization path.}
#'    \item{population.storage \code{list}}{List of populations.}
#'    \item{message \code{character(1)}}{Message explaining the reason for termination.}
#'   }
#' @examples
#' library(smoof)
#' library(ParamHelpers)
#' # We want to find the minimum of the function f(x) = x sin(2x) on the intervall
#' # [0, 2pi]. The optimal value is about -5.5 for x = 5.54.
#' # First we wrap the function with the smoof package:
#' obj.fn = makeSingleObjectiveFunction(
#'   name = "My obj. function",
#'   fn = function(x) x * sin(2 * x),
#'   par.set = makeParamSet(makeNumericParam("x", lower = 0, upper = 2 * pi))
#' )
#'
#' # We want to solve this with a (10 + 10) evolutionary strategy based on
#' # the floating point representation of the input vectors with the default
#' # operators: intermediate recombinator and Gauss mutation
#' ctrl = ecr.control(
#'   population.size = 10L,
#'   offspring.size = 10L,
#'   survival.strategy = "plus",
#'   n.params = 1L,
#'   representation = "float",
#'   stopping.conditions = setupStoppingConditions(max.iter = 100L)
#' )
#'
#' res = ecr(obj.fn, control = ctrl)
#' print(res)
#'
#' @seealso \code{\link{ecr.control}}
#' @export
ecr = function(objective.fun, control) {
  assertClass(objective.fun, "smoof_function")
  par.set = getParamSet(objective.fun)
  assertClass(par.set, "ParamSet")

  n.params = control$n.params
  max.iter = control$max.iter
  max.time = control$max.time
  population.size = control$population.size
  mating.pool.size = control$mating.pool.size
  offspring.size = control$offspring.size
  termination.eps = control$termination.eps
  monitor = control$monitor

  # potentially global optimum
  global.optimum = NULL
  if (hasGlobalOptimum(objective.fun)) {
    global.optimum = getGlobalOptimum(objective.fun)$param
  }

  if (control$representation %in% c("float") && !hasFiniteBoxConstraints(par.set)) {
    stopf("Lower and upper box constraints needed for representation type 'float'.")
  }

  lower = getLower(par.set)
  upper = getUpper(par.set)

  populationGenerator = control$generator
  matingPoolGenerator = control$mating.pool.generator

  population = populationGenerator(population.size, n.params, lower, upper, control)
  population$fitness = computeFitness(population, objective.fun)
  best = getBestIndividual(population)

  opt.path = makeOptPathDF(par.set, y.names = "y", minimize = TRUE, include.extra = TRUE)
  opt.path = addBestToOptPath(opt.path, par.set, best, population$fitness, 0)

  population.storage = namedList(control$save.population.at)
  if (0 %in% control$save.population.at) {
    population.storage[[as.character(0)]] = population
  }

  iter = 1L
  start.time = Sys.time()

  monitor$before()

  repeat {
    monitor$step()

    parents = matingPoolGenerator(population, mating.pool.size)
    offspring = generateOffspring(parents, objective.fun, control)

    population = selectForSurvival(
      population,
      offspring,
      population.size,
      strategy = control$survival.strategy,
      elite.size = control$elite.size)

    if (iter %in% control$save.population.at) {
      population.storage[[as.character(iter)]] = population
    }

    best = getBestIndividual(population)
    opt.path = addBestToOptPath(opt.path, par.set, best, population$fitness, iter)

    stop.object = doTerminate(control$stopping.conditions)
    if (length(stop.object) > 0L) {
      break
    }

    iter = iter + 1
  }

  monitor$after()

  return(
    structure(list(
      objective.fun = objective.fun,
      control = control,
      best.param = setColNames(t(data.frame(best$individual)), getParamIds(par.set, repeated = TRUE, with.nr = TRUE)),
      best.value = best$fitness,
      opt.path = opt.path,
      population.storage = population.storage,
      message = stop.object$message
    ), class = "ecr_result")
  )
}

#' Print the result of an ecr run.
#'
#' @param x [\code{ecr_result}]\cr
#'   ecr result object.
#' @param ... [any]\cr
#'   Not used.
#' @export
print.ecr_result = function(x, ...) {
  opt.path = x$opt.path
  par.set = opt.path$par.set
  catf("Parameters: %s", paste(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), "=", x$best.param, sep = "", collapse = ", "))
  catf("Objective function value: %s\n", paste(x$control$target.name, "=", x$best.value, sep ="", collapse = ", "))
}

# Adds the parameter values and the y-value(s) of the best individual to the opt.path.
#
# @param opt.path [\code{\link[ParamHelpers]{OptPathDF}}]\cr
#   Optimization path.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param best [\code{setOfIndividuals}]\cr
#   Best individual in the current generation/iteration.
# @param fitness [\code{numeric}]\cr
#   Numeric vector of fitness values for the current generation.
# @param generation [\code{integer(1)}]\cr
#   Current generation.
# @return [\code{\link[ParamHelpers]{OptPathDF}}]
addBestToOptPath = function(opt.path, par.set, best, fitness, generation) {
  if (length(par.set$pars) == 1L) {
    best.param.values = list(best$individual)
  } else {
    best.param.values = as.list(best$individual)
    names(best.param.values) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  }
  extras = list(
    pop.min.fitness = min(fitness),
    pop.mean.fitness = mean(fitness),
    pop.median.fitness = median(fitness),
    pop.max.fitness = max(fitness)
  )
  addOptPathEl(opt.path, x = best.param.values, y = best$fitness, dob = generation, extra = extras)
  return(opt.path)
}
