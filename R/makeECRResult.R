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
  task, best, opt.path, storage, control,
  population.storage = NULL, stop.object) {
  makeS3Obj(
    task = task,
    control = control,
    best.param = best$individual,
    best.value = best$fitness,
    storage = storage,
    opt.path = opt.path,
    population.storage = population.storage,
    message = stop.object$message,
    classes = "ecr_single_objective_result"
  )
}

#' @export
print.ecr_single_objective_result = function(x, ...) {
  minmax = ifelse(x$task$minimize, "minimization", "maximization")
  catf("EA applied to solve single-objective %s problem.", minmax)
  catf("Best found value: %.6f", x$best.value)
  if (isSmoofFunction(x$task$fitness.fun)) {
    n = length(x$best.param)
    l = min(n, 1L)
    pars = collapse(x$best.param[seq(l)], sep = ", ")
    if (l < n)
      pars = paste(pars, ", ...")
    catf("Best found parameters: %s", pars)
  }
  printAdditionalInformation(x)
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
  task, opt.path, storage, control,
  population.storage, stop.object) {
  max.dob = max(getOptPathDOB(opt.path))
  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE, dob = max.dob)
  makeS3Obj(
    task = task,
    control = control,
    opt.path = opt.path,
    storage = storage,
    pareto.front = getOptPathY(opt.path)[pareto.inds, , drop = FALSE],
    pareto.set = lapply(pareto.inds, function(i) getOptPathEl(opt.path, i)$x),
    pareto.inds = pareto.inds,
    message = stop.object$message,
    classes = "ecr_multi_objective_result"
  )
}

#' @export
print.ecr_multi_objective_result = function(x, ...) {
  obj = ifelse(x$task$n.objectives == 2L, "bi-objective", "many-objective")
  catf("EA applied to solve %s problem.\n", obj)
  catf("Number of nondominanted points: %i", nrow(x$pareto.front))
  printAdditionalInformation(x)
}

# @title
#   Summary function for multi objective ecr result.
#
# @param object [\code{ecr_multi_objective_result}]\cr
#   Result object.
# @param ref.points [\code{matrix}]\cr
#   Matrix of reference points (one point per column) used for the emoa quality
#   indicators, i.e., epsilon indicator and hypervolume indicator.
# @return [\code{ecr_multi_objective_result_summary}]
# @export
summary.ecr_multi_objective_result = function(object, ref.points = NULL, ...) {
  # convert the data frame to matrix
  pf = t(object$pareto.front)
  #FIXME: maybe add the possibility to pass ref point to control and use it here?
  makeS3Obj(
    n.nondom = ncol(pf),
    dom.hv = computeDominatedHypervolume(pf),
    eps.ind = if (!is.null(ref.points)) computeEpsilonIndicator(pf, ref.points) else NA,
    hv.ind = if (!is.null(ref.points)) computeHypervolumeIndicator(pf, ref.points) else NA,
    classes = c("list", "ecr_multi_objective_result_summary")
  )
}

# @export
print.ecr_multi_objective_result_summary = function(x, ...) {
  print(as.data.frame(x))
}

printAdditionalInformation = function(x) {
  catf(x$message)
  catf("Generations: %i", max(getOptPathCol(x$opt.path, "iter")))
  catf("Evaluations: %i", max(getOptPathCol(x$opt.path, "n.evals")))
}
