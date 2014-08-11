#' Working horse of the ecr package.
#'
#' Takes a function and searches for a global optimum with an evolutionary approach.
#'
#' @param objective.fun [\code{otf_function}]\cr
#'   Single objective target function of type \code{otf_function}.
#' @param control [\code{ecr.control}]\cr
#'   Control object.
#' return [\code{ecrResult}]
#'   Object of type \code{ecr_result} containing a list:
#'   \itemize{
#'    \item{best.param \code{numeric}}{Best parameter combination.}
#'    \item{best.value \code{numeric(1)}}{Best reached value.}
#'    \item{opt.path \code{optPath}}{Optimization path.}
#'    \item{population.storage \code{list}}{List of populations.}
#'    \item{convergence \code{integer(1)}}{Termination status code.}
#'    \item{message \code{character(1)}}{Message explaining the reason for termination.}
#'   }
#' @export
ecr = function(objective.fun, control) {
  assertClass(objective.fun, "otf_function")
  par.set = getParamSet(objective.fun)
  assertClass(par.set, "ParamSet")

  n.params = control$n.params
  max.iter = control$max.iter
  max.time = control$max.time
  population.size = control$population.size
  mating.pool.size = control$mating.pool.size
  offspring.size = control$offspring.size
  show.info = control$show.info
  show.info.stepsize = control$show.info.stepsize
  termination.eps = control$termination.eps
  monitor = control$monitor

  # potentially global optimum
  global.optimum = NULL
  if (hasGlobalOptimum(objective.fun)) {
    global.optimum = getGlobalOptimum(objective.fun)$param
  }

  if (n.params != length(par.set$pars)) {
    stopf("Number of parameters given by control object and ParamSet do not match: %i != %i", n.params, length(par.set$pars))
  }

  if (control$representation %in% c("float") && !hasFiniteBoxConstraints(par.set)) {
    stopf("Lower and upper box constraints needed for representation type 'float'.")
  }

  lower = getLower(par.set)
  upper = getUpper(par.set)

  populationGenerator = control$generator
  matingPoolGenerator = control$mating.pool.generator

  population = populationGenerator(population.size, n.params, lower, upper, control)
  population = computeFitness(population, objective.fun)
  best = getBestIndividual(population)

  opt.path = makeOptPathDF(par.set, y.names = "y", minimize = TRUE)
  opt.path = addBestToOptPath(opt.path, par.set, best, 0)

  population.storage = namedList(control$save.population.at)
  if (0 %in% control$save.population.at) {
    population.storage[[as.character(0)]] = population
  }

  iter = 1L
  start.time = Sys.time()

  if (show.info)
    monitor$before(objective.fun, population, trace, iter, control)

  repeat {
    if (show.info && (iter %% show.info.stepsize == 0L))
      monitor$step(objective.fun, population, trace, iter, control)

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
    opt.path = addBestToOptPath(opt.path, par.set, best, iter)

    termination.code = getTerminationCode(iter, max.iter, global.optimum, best, termination.eps, start.time, max.time)
    if (termination.code >= 0) {
      break
    }

    iter = iter + 1
  }

  if (show.info)
    monitor$after(objective.fun, population, trace, iter, control)

  return(
    structure(list(
      best.param = setColNames(t(data.frame(best$individual)), getParamIds(par.set, repeated = TRUE, with.nr = TRUE)),
      best.value = best$fitness,
      opt.path = opt.path,
      population.storage = population.storage,
      convergence = termination.code,
      message = getTerminationMessage(termination.code)
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
  #FIXME: this is ugly! But paramValueAsString does not work for some reason.
  catf("Parameters: %s", paste(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), "=", x$best.param, sep = "", collapse = ", "))
  catf("Objective function value: %s\n", paste(x$target.names, "=", x$best.value, sep ="", collapse = ", "))

  catf("Optimization path:")
  opt.path = as.data.frame(opt.path)
  print(head(opt.path, 10))
  catf("...")
  print(tail(opt.path, 10))
}

# Adds the parameter values and the y-value(s) of the best individual to the opt.path.
#
# @param opt.path [\code{\link[ParamHelpers]{OptPathDF}}]\cr
#   Optimization path.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param best [\code{ecr_individual}]\cr
#   Best individual in the current generation/iteration.
# @param generation [\code{integer(1)}]\cr
#   Current generation.
# @return [\code{\link[ParamHelpers]{OptPathDF}}]
addBestToOptPath = function(opt.path, par.set, best, generation) {
  #FIXME: until now we only consider mono-criteria stuff
  best.param.values = dfRowToList(data.frame(t(best$individual)), par.set, 1)
  addOptPathEl(opt.path, x = best.param.values, y = best$fitness, dob = generation)
  return(opt.path)
}
