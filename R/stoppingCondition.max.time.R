#' Stopping condition: time limit.
#'
#' @param max.time [\code{integer(1)}]\cr
#'   Time budget in seconds. Default ist \code{Inf}.
#' @return [\code{function}]
#' @export
makeMaximumTimeStoppingCondition = function(max.time = NULL) {
  if (!is.null(max.time)) {
    assertInt(max.time, lower = 1L, na.ok = FALSE)
  } else {
    max.time = Inf
  }
  force(max.time)

  condition.fun = function(opt.path) {
    times.vec = getOptPathCol(opt.path, "past.time")
    max(times.vec) >= max.time
  }

  makeStoppingCondition(
    condition.fun,
    name = "TimeLimit",
    message = sprintf("Time limit reached: '%s' [seconds]", max.time)
  )
}
