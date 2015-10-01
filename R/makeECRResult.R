#' Single objective result object.
#'
#' Object returned by \code{\link{doTheEvolution}} in case of the objective function
#' being single-objective.
#'
#' The S3 object containts the following members:
#' \describe{
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{control}{The \code{ecr_control} object passed to \code{\link{doTheEvolution}}.}
#'   \item{best.param}{Overall best parameters.}
#'   \item{best.value}{Overall best objective value.}
#'   \item{storage}{List of additional stuff saved within the evolutionary process (EA dependent).}
#'   \item{opt.path}{Optimization path \code{\link[ParamHelpers]{OptPath}}.}
#'   \item{population.storage}{Named list of populations stored during the process.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#' @name ECRSingleObjectiveResult
#' @rdname ECRSingleObjectiveResult
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
    classes = c("ecr_single_objective_result", "ecr_result")
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
#' being multi-objective.
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
    classes = c("ecr_multi_objective_result", "ecr_result")
  )
}

#' @export
print.ecr_multi_objective_result = function(x, ...) {
  obj = ifelse(x$task$n.objectives == 2L, "bi-objective", "many-objective")
  catf("EA applied to solve %s problem.\n", obj)
  catf("Number of nondominanted points: %i", nrow(x$pareto.front))
  printAdditionalInformation(x)
}

#' @title
#'   Summary function for multi objective ecr result.
#'
#' @param object [\code{ecr_multi_objective_result}]\cr
#'   Result object.
#' @param ref.points [\code{matrix}]\cr
#'   Matrix of reference points (one point per column) used for the emoa quality
#'   indicators, i.e., epsilon indicator and hypervolume indicator.
#' @param ... [any]\cr
#'   Furhter parameters passed to R\{1,2,3\} computation functions. See e.g.
#'   \code{\link{computeR1Indicator}} for details.
#' @return [\code{ecr_multi_objective_result_summary}]
#' @export
summary.ecr_multi_objective_result = function(object, ref.points = NULL, ...) {
  # convert the data frame to matrix
  pf = t(object$pareto.front)
  makeS3Obj(
    n.nondom = ncol(pf),
    dom.hv = computeDominatedHypervolume(pf),
    eps.ind = if (!is.null(ref.points)) computeEpsilonIndicator(pf, ref.points) else NA,
    hv.ind = if (!is.null(ref.points)) computeHypervolumeIndicator(pf, ref.points) else NA,
    r1.ind = if (!is.null(ref.points)) computeR1Indicator(pf, ref.points, ...) else NA,
    r2.ind = if (!is.null(ref.points)) computeR2Indicator(pf, ref.points, ...) else NA,
    r3.ind = if (!is.null(ref.points)) computeR3Indicator(pf, ref.points, ...) else NA,
    classes = c("list", "ecr_multi_objective_result_summary")
  )
}

# @export
print.ecr_multi_objective_result_summary = function(x, ...) {
  print(as.data.frame(x))
}

printAdditionalInformation = function(x) {
  catf(x$message)
  catf("Generations: %i", getGenerations(x))
  catf("Evaluations: %i", getEvaluations(x))
}

#' @title Determine number of function evaluations needed.
#'
#' @param result [\code{ecr_result}]\cr
#'   \pkg{ecr} result object.
#' @return [integer(1)]
#' @export
getEvaluations = function(result) {
  assertClass(result, "ecr_result")
  return(max(getOptPathCol(result$opt.path, "n.evals")))
}

#' @title Determine number of generations.
#'
#' @param result [\code{ecr_result}]\cr
#'   \pkg{ecr} result object.
#' @return [integer(1)]
#' @export
getGenerations = function(result) {
  return(max(getOptPathCol(result$opt.path, "iter")))
}
