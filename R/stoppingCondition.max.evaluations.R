#' @title
#'   Stopping condition: maximum number of function evaluations.
#'
#' @param max.evals [\code{integer(1)}]\cr
#'   Maximal number of function evaluations. Default ist \code{Inf}.
#' @return [\code{function}]
#' @export
makeMaximumEvaluationsStoppingCondition = function(max.evals = NULL) {
  if (!is.null(max.evals)) {
    assertInt(max.evals, lower = 1L, na.ok = FALSE)
  } else {
    max.evals = Inf
  }
  force(max.evals)

  condition.fun = function(opt.path) {
    vals = getOptPathCol(opt.path, "n.evals")
    return(vals[length(vals)] >= max.evals)
  }

  makeStoppingCondition(
    condition.fun,
    name = "FunctionEvaluationsLimit",
    message = sprintf("Maximum number of objective function evaluations reached: '%i'", max.evals)
  )
}
