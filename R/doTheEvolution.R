#' Working horse of the ecr package.
#'
#' Takes a function and searches for a global optimum with an evolutionary approach.
#'
#' @param objective.fun [\code{smoof_function}]\cr
#'   Single objective target function of type \code{smoof_function}.
#' @param control [\code{setupECRControl}]\cr
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
#' @example examples/ex_doTheEvolution.R
#' @seealso \code{\link{setupECRControl}}
#' @export
#FIXME: get rid of the global optimum stuff. Only useful for single-objective termination
# criterion
#FIXME: addBestToOptPath is not a proper name since in the mo case, there is in general no
# single best. Maybe call it better updateOptPath
#FIXME: for standard representations: save all stuff in opt.path, i.e., make opt path the
# population storage?
#FIXME: optPath funs need option to make x and par.set optional
#FIXME: we can extract the number of objectives from the smoof function, but what do we
# do if we use custom representations and there is not par.set? We should force the user to pass
# n.objectives to the control object!
#FIXME: for custom representations we need to store the the best parameter since it cannot be
# saved in the opt path :-(
doTheEvolution = function(objective.fun, control) {
  repr = control$representation
  par.set = NULL
  n.objectives = 1L
  if (repr != "custom") {
    assertClass(objective.fun, "smoof_function")
    par.set = getParamSet(objective.fun)
    n.objectives = getNumberOfObjectives(objective.fun)

    #FIXME: is this a good idea to modify control object here?
    control$par.set = par.set
    control$n.objectives = n.objectives
    control$par.lower = getLower(par.set, with.nr = TRUE)
    control$par.upper = getUpper(par.set, with.nr = TRUE)

    # potentially global optimum
    #FIXME: actually we to not use this here and it is only relevant for the
    # the corresponding stopping condition.
    global.optimum = NULL
    if (hasGlobalOptimum(objective.fun)) {
      global.optimum = getGlobalOptimum(objective.fun)$param
    }

    if (repr == "float" && !hasFiniteBoxConstraints(par.set)) {
      stopf("Lower and upper box constraints needed for representation type 'float'.")
    }
  } else {
    # dummy par.set
    control$par.set = makeParamSet(makeNumericParam("dummy", lower = 0, upper = 1))
  }

  # check compatibility of selectors and #objectives
  selectors = c(control$parent.selector, control$survival.selector)
  desired.tag = if (n.objectives == 1L) "single-objective" else "multi-objective"
  lapply(selectors, function(selector) {
    if (desired.tag %nin% attr(selector, "supported.objectives")) {
      stopf("Selector '%s' cannot be applied to problem with %i objectives.",
        getOperatorName(selector), n.objectives)
    }
  })

  # extract basic information
  y.names = paste0("y", seq(n.objectives))
  n.population = control$n.population
  n.mating.pool = control$n.mating.pool
  n.offspring = control$n.offspring
  monitor = control$monitor

  # extract generator and selector
  populationGenerator = control$generator
  parentSelector = control$parent.selector

  # init some vars
  iter = 1L
  start.time = Sys.time()

  # generate intial population
  population = populationGenerator(n.population, control)
  population$fitness = computeFitness(population, objective.fun)
  pop.gen.time = difftime(Sys.time(), start.time, units = "secs")

  # initialize trace (depends on #objectives)
  trace = initTrace(control, population, n.objectives, y.names)
  trace = updateTrace(trace, iter, population, start.time, pop.gen.time, control)

  population.storage = namedList(paste0("gen.", control$save.population.at))
  # store start population
  if (0 %in% control$save.population.at) {
    population.storage[[paste0("gen.", as.character(0))]] = population
  }

  monitor$before()

  repeat {
    # monitoring
    monitor$step()

    # measure time of offspring generation
    off.gen.start.time = Sys.time()

    # actually create offspring
    matingPool = parentSelector(population, n.mating.pool)
    offspring = generateOffspring(matingPool, objective.fun, control, trace$opt.path)

    # apply survival selection and set up the (i+1)-th generation
    population = selectForSurvival(
      population,
      offspring,
      n.population,
      strategy = control$survival.strategy,
      n.elite = control$n.elite,
      control
    )

    off.gen.time = difftime(Sys.time(), off.gen.start.time, units = "secs")

    # some bookkeeping
    if (iter %in% control$save.population.at) {
      population.storage[[paste0("gen.", as.character(iter))]] = population
    }
    trace = updateTrace(trace, iter, population, start.time, off.gen.time, control)

    # check if any termination criterion is met
    stop.object = doTerminate(control$stopping.conditions, trace$opt.path)
    if (length(stop.object) > 0L) {
      break
    }

    iter = iter + 1
  }

  monitor$after()

  # generate result object
  if (n.objectives == 1L) {
    makeECRSingleObjectiveResult(objective.fun, trace$best, trace$opt.path, control,
      population.storage, stop.object)
  } else {
    makeECRMultiObjectiveResult(objective.fun, trace$opt.path, control,
      population.storage, stop.object)
  }
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
  catf("Parameters: %s", paste(getParamIds(par.set, repeated = TRUE, with.nr = TRUE),
    "=", x$best.param, sep = "", collapse = ", "))
  catf("Objective function value: %s\n", paste(x$control$target.name, "=",
    x$best.value, sep ="", collapse = ", "))
}

# Generate 'extras' argument for opt.path.
#
# @param iter [numeric(1)]
#   Current iteration/generation.
# @param population [ecr_population]
#   Current population.
# @param start.time [POSIXct]
#   Start time of evolution process.
# @pram control [ecr_control]
#   Control object.
# @return [list] Named list with scalar values to be stored in opt.path.
getListOfExtras = function(iter, population, start.time, control) {
  fitness = population$fitness
  extra = list(
    past.time = as.numeric(Sys.time() - start.time),
    iter = iter,
    pop.min.fitness = min(fitness),
    pop.mean.fitness = mean(fitness),
    pop.median.fitness = median(fitness),
    pop.max.fitness = max(fitness)
  )
  # compute and log used defined stuff
  if (!is.null(control$extras.fun)) {
    user.extra = control$extras.fun(population)
    if (!testList(user.extra, names = "strict")) {
      stopf("Result computed by 'extras.fun' is not a named list!")
    }
    extra = c(extra, user.extra)
  }
  return(extra)
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
addBestToOptPath = function(opt.path, par.set, best, fitness, generation, exec.time, extra, control) {
  if (!is.null(control$n.objectives) && control$n.objectives > 1L) {
    best.param.values = as.list(rep(0, sum(getParamLengths(par.set))))
    names(best.param.values) = getParamIds(par.set)
  } else if (length(par.set$pars) == 1L) {
    best.param.values = list(best$individual)
    names(best.param.values) = getParamIds(par.set)
  } else {
    best.param.values = as.list(best$individual)
    names(best.param.values) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  }
  #FIXME: dummy value for custom representation
  if (control$representation == "custom") {
    best.param.values = list("x" = 0.5)
  }
  #FIXME: since there is no "best" individual in the multi-objective case we save
  # a dummy here for now. We need to think about what to save in this case? Save all
  # individuals of every population?
  if (!is.null(control$n.objectives) && control$n.objectives > 1L) {
    y = rep(0.0, control$n.objectives)
  } else {
    y = unlist(best$fitness)
  }
  addOptPathEl(opt.path, x = best.param.values, y = y, dob = generation,
    exec.time = exec.time, extra = extra, check.feasible = FALSE)
  return(opt.path)
}
