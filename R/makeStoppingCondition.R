doTerminate = function(stopping.funs, envir = parent.frame()) {
    # if we have not specified any stopping conditions always return false
    if (!length(stopping.funs)) {
        return(FALSE)
    }

    # otherwise iterate over stopping conditions and check
    stopObject = list()
    for (stopping.fun in stopping.funs) {
        shouldStop = stopping.fun(envir = envir)
        if (shouldStop) {
            stopObject$name = attr(stopping.fun, "name")
            stopObject$description = attr(stopping.fun, "description")
            break
        }
    }
    return(stopObject)
}

#' Maximum time stopping condition.
#'
#' @param max.time [\code{integer(1)}]\cr
#'   Time budget in seconds. Default ist \code{Inf}.
#' @return [\code{function}]
makeMaximumTimeStoppingCondition = function(max.time) {
    assertCount(max.time, positive = TRUE, na.ok = FALSE)
    force(max.time)

    condition.fun = function(envir = parent.frame()) {
        time =  Sys.time()
        timediff = difftime(time, envir$start.time, units = "secs")
        timediff >= max.time
    }

    makeStoppingCondition(condition.fun, name = "TimeLimit",
        description = sprintf("Time limit '%s' [secs] reached.", max.time))
}

makeStoppingCondition = function(condition.fun, name, description) {
    assertFunction(condition.fun, args = c("envir"))
    attr(condition.fun, "name") = name
    attr(condition.fun, "description") = description
    return(condition.fun)
}