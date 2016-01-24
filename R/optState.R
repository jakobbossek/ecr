#' @title
#' Optimization state.
#'
#' @description
#' Description is coming soon ...
#'
#' @name OptState
#' @rdname OptState
NULL

# @title Generate an optimization state object.
#
# @description The optimization task object is a container for all variables
# regarding the optimization process.
#
# @param task [\code{ecr_optimization_task}]\cr
#   Optimization task.
# @param population [\code{ecr_population}]\cr
#   (Initial) population.
# @param control [\code{ecr_control}]\cr
#   Control object.
# @return [\code{ecr_opt_state}]
setupOptState = function(task, population, control) {
  opt.state = new.env()
  opt.state$iter = 0L
  opt.state$time.created = Sys.time()
  opt.state$time.passed = 0
  opt.state$task = task
  opt.state$par.set = task$par.set
  opt.state$n.evals = control$n.population
  opt.state$control = control

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    best = getBestIndividual(population, task)
    opt.state$best.param = best$individual
    opt.state$best.value = best$fitness
  }

  # construct opt path
  y.names = paste0("y", seq(task$n.objectives))

  # EA specific
  opt.state$population = population
  obj.class = paste("ecr", ifelse (task$n.objectives == 1L, "single_objective", "multi_objective"), "opt_state", sep = "_")
  class(opt.state) = c("ecr_opt_state", obj.class, class(opt.state))
  return(opt.state)
}

# @title Update optimization state.
#
# @decscription Called once after survival selection to update the optimization
# state.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
# @param population [\code{ecr_population}]\cr
#   Current population.
# @param control [\code{ecr_control}]\cr.
#   Control object.
updateOptState = function(opt.state, population, control) {
  task = opt.state$task

  # update stuff
  opt.state$iter = opt.state$iter + 1L
  opt.state$n.evals = opt.state$n.evals + control$n.offspring
  opt.state$time.passed = difftime(Sys.time(), opt.state$time.created, units = "secs")

  # update population
  opt.state$population = population

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    updateOptStateBestIndividual(opt.state)
  }

  invisible()
}

# @title Update best individual.
#
# @description Select the best individual of the current population and eventually
# replace the best so far.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
updateOptStateBestIndividual = function(opt.state) {
  task = opt.state$task

  population.best = getBestIndividual(opt.state)
  #FIXME: ugly as sin! Redundant code
  if (task$minimize) {
    if (population.best$fitness <= opt.state$best.value) {
      opt.state$best.param = population.best$individual
      opt.state$best.value = population.best$fitness
    }
  } else {
    if (population.best$fitness >= opt.state$best.value) {
      opt.state$best.param = population.best$individual
      opt.state$best.value = population.best$fitness
    }
  }
  invisible()
}

#' @title
#' Optimization state getter functions.
#'
#' @description
#' The \code{\link{OptState}} is generated internally by \code{\link{doTheEvolution}},
#' but it is exposed to the user in some cases. E.g., self-defined actions or
#' stopping conditions get the current \code{\link{OptState}} passed as an argument.
#' The following collection of simple getter functions helps to access the
#' properties of the optimization state.
#'
#' @param opt.state [\code{\link{OptState}}]\cr
#'   Optimization state.
#' @return [any] Type depends on the property.
#' @name OptStateGetter
#' @rdname OptStateGetter
#' @export
getOptStateCurrentIter = function(opt.state) {
  return(opt.state$iter)
}

#' @rdname OptStateGetter
#' @export
getOptStateTask = function(opt.state) {
  return(opt.state$task)
}

#' @rdname OptStateGetter
#' @export
getOptStateParamSet = function(opt.state) {
  return(opt.state$par.set)
}

#' @rdname OptStateGetter
#' @export
getOptStateCurrentEvaluations = function(opt.state) {
  return(opt.state$n.evals)
}

#' @rdname OptStateGetter
#' @export
getOptStateControl = function(opt.state) {
  return(opt.state$control)
}

#' @rdname OptStateGetter
#' @export
getOptStateBestIndividual = function(opt.state) {
  if (getOptStateTask(opt.state)$n.objectives > 1L) {
    stopf("OptState: no 'best' individual in multi-objective optimization task.")
  }
  return(list(
    param = opt.state$best.param,
    value = opt.state$best.value
  ))
}

#' @rdname OptStateGetter
#' @export
getOptStatePopulation = function(opt.state) {
  return(opt.state$population)
}

#' @rdname OptStateGetter
#' @export
getOptStateFitness = function(opt.state) {
  return(getOptStatePopulation(opt.state)$fitness)
}

#' @rdname OptStateGetter
#' @export
getOptStateTimePassed = function(opt.state) {
  return(opt.state$time.passed)
}

#' @rdname OptStateGetter
#' @export
getOptStateOptPath = function(opt.state) {
  if (is.null(opt.state$opt.path)) {
    stopf("No ParamHelpers::OptPath available. You need to activate the OptPathLoggingMonitor.")
  }
  return(opt.state$opt.path)
}
