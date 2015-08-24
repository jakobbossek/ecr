#' @title
#'   Stopping condition: maximum number of iterations.
#'
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @return [\code{function}]
#' @export
makeMaximumIterationsStoppingCondition = function(max.iter = NULL) {
  if (!is.null(max.iter)) {
    assertInt(max.iter, lower = 1L, na.ok = FALSE)
  } else {
    max.iter = Inf
  }
  force(max.iter)

  condition.fun = function(opt.path) {
    iter.vector = getOptPathCol(opt.path, "iter")
    return(max(iter.vector) >= max.iter)
  }

  makeStoppingCondition(
    condition.fun,
    name = "IterLimit",
    message = sprintf("Maximum number of iterations reached: '%i'", max.iter)
  )
}
