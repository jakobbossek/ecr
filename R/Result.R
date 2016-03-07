#' @title
#' Result object.
#'
#' @description
#' S3 object returned by \code{\link{doTheEvolution}} containing the best found
#' parameter setting and value in the single-objective case and the Pareto-front/-set
#' in case of a multi-objective optimization problem. Moreover a set of further
#' information, e.g., reason of termination, the control object etc. are returned.
#'
#' The single objective result object contains the following fields:
#' \describe{
#'   \item{final.opt.state}{The last optimization state.}
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{control}{The \code{ecr_control} object passed to \code{\link{doTheEvolution}}.}
#'   \item{best.param}{Overall best parameter setting.}
#'   \item{best.value}{Overall best objective value.}
#'   \item{opt.path}{Optimization path \code{\link[ParamHelpers]{OptPath}}.}
#'   \item{last.population}{Last population.}
#'   \item{population.storage}{Named list of populations stored during the process.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#'
#' In case of a solved multi-objective function the result object contains the
#' following fields:
#' \describe{
#'   \item{final.opt.state}{The last optimization state.}
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{control}{The \code{ecr_control} object passed to \code{\link{doTheEvolution}}.}
#'   \item{pareto.idx}{Indizes of the non-dominated solutions in the last population.}
#'   \item{pareto.front}{(n x d) matrix of the approximated non-dominated front where n
#'   is the number of non-dominated points and d is the number of objectives.}
#'   \item{pareto.set}{Matrix of decision space values resulting with objective values
#'   given in pareto.front.}
#'   \item{last.population}{Last population.}
#'   \item{population.storage}{Named list of populations stored during the process.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#'
#' @name ecr_result
#' @rdname ecr_result
NULL

# @title Generator for result object.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
# @param stop.object [\code{list}]\cr
#   List of triggered stopping conditions.
# @param control [\code{ecr_control}]\cr
#   Control object.
# @return [\code{ecr_single_objective_result} | \code{ecr_multi_objective_result}]
setupResult = function(opt.state, stop.object, control) {
  UseMethod("setupResult")
}

#' @export
setupResult.ecr_single_objective_opt_state = function(opt.state, stop.object, control) {
  makeS3Obj(
    final.opt.state = opt.state,
    task = opt.state$task,
    control = control,
    best.param = opt.state$best.param,
    best.value = opt.state$best.value,
    opt.path = opt.state$opt.path,
    last.population = opt.state$population,
    population.storage = opt.state$population.storage,
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

#' @export
setupResult.ecr_multi_objective_opt_state = function(opt.state, stop.object, control) {
  population = opt.state$population
  fitness = population$fitness
  pareto.idx = which.nondominated(fitness)
  pareto.front = t(fitness[, pareto.idx, drop = FALSE])
  colnames(pareto.front) = opt.state$task$objective.names
  makeS3Obj(
    final.opt.state = opt.state,
    task = opt.state$task,
    control = control,
    opt.path = opt.state$opt.path,
    pareto.idx = pareto.idx,
    pareto.front = t(fitness[, pareto.idx, drop = FALSE]),
    pareto.set = population[pareto.idx],
    last.population = population,
    population.storage = opt.state$population.storage,
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
#' Summary function for multi objective ecr result.
#'
#' @description
#' Computes a table of EMOA indicators based on a multi-objective ecr result object.
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
  catf("Generations: %i", x$final.opt.state$iter)
  catf("Evaluations: %i", x$final.opt.state$n.evals)
}

#' @title
#' Get number of function evaluations.
#'
#' @description
#' Determine the number of function evaluations needed by the EA.
#'
#' @param result [\code{ecr_result}]\cr
#'   \pkg{ecr} result object.
#' @return [integer(1)]
#' @export
getEvaluations = function(result) {
  assertClass(result, "ecr_result")
  return(result$final.opt.state$n.evals)
}

#' @title
#' Get number of generations.
#'
#' @description
#' Determine the number of function evaluations needed by the EA.
#'
#' @param result [\code{ecr_result}]\cr
#'   \pkg{ecr} result object.
#' @return [integer(1)]
#' @export
getGenerations = function(result) {
  return(result$final.opt.state$iter)
}
