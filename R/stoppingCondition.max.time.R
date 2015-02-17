#' Stopping condition: time limit.
#'
#' @param max.time [\code{integer(1)}]\cr
#'   Time budget in seconds. Default ist \code{Inf}.
#' @return [\code{function}]
#' @export
makeMaximumTimeStoppingCondition = function(max.time) {
    assertCount(max.time, positive = TRUE, na.ok = FALSE)
    force(max.time)

    condition.fun = function(envir = parent.frame()) {
        time = Sys.time()
        #FIXME: hmm, somehow this is ugly with the environments. We need to
        # believe, that this stuff (like start.time) exists.
        timediff = difftime(time, envir$start.time, units = "secs")
        timediff >= max.time
    }

    makeStoppingCondition(
        condition.fun,
        name = "TimeLimit",
        message = sprintf("Time limit reached: '%s' [seconds]", max.time)
    )
}
