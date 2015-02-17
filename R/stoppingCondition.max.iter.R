#' Stopping condition: maximum number of iterations.
#'
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @return [\code{function}]
#' @export
makeMaximumIterationsStoppingCondition = function(max.iter = Inf) {
    assertCount(max.iter, positive = TRUE, na.ok = FALSE)
    force(max.iter)

    condition.fun = function(envir = parent.frame()) {
        envir$iter > max.iter
    }

    makeStoppingCondition(
        condition.fun,
        name = "IterLimit",
        message = sprintf("Maximum number of iterations reached: '%i'", max.iter)
    )
}
