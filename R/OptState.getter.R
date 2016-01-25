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
