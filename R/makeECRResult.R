#' Single objective result object.
#'
#' Object returned by \code{\link{doTheEvolution}} in case of the objective function
#' being single-objective.
#'
#' It contains ...
#' @name ECRSingleObjectiveResult
#' @rdname ECRSingleObjectiveResult
#FIXME: finish this
NULL

makeECRSingleObjectiveResult = function(
  objective.fun, best, opt.path, control,
  population.storage = NULL, stop.object) {
  makeS3Obj(
    objective.fun = objective.fun,
    control = control,
    best.param = best$individual,
    best.value = best$fitness,
    opt.path = opt.path,
    population.storage = population.storage,
    message = stop.object$message,
    classes = "ecr_single_objective_result"
  )
}

#' Multi objective result object.
#'
#' Object returned by \code{\link{doTheEvolution}} in case of the objective function
#' being single-objective.
#'
#' It contains ...
#' @name ECRMultiObjectiveResult
#' @rdname ECRMultiObjectiveResult
#FIXME: finish this
NULL

makeECRMultiObjectiveResult = function(
  objective.fun, opt.path, control,
  population.storage, stop.object) {
  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE)
  makeS3Obj(
    objective.fun = objective.fun,
    control = control,
    opt.path = opt.path,
    pareto.front = getOptPathY(opt.path)[pareto.inds, , drop = FALSE],
    pareto.set = lapply(pareto.inds, function(i) getOptPathEl(opt.path, i)$x),
    pareto.inds = pareto.inds,
    classes = "ecr_multi_objective_result"
  )
}
