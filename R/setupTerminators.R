#' @title
#' Generator for some frequently used stopping conditions.
#'
#' @description
#' Setting up stopping conditions is flexible and straight-forward, but
#' it needs a lot of writing. This function generates a list of frequently
#' used stopping conditions with just a single function call.
#'
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @param max.time [\code{integer(1)}]\cr
#'   Time budget in seconds. Default ist \code{Inf}.
#' @return [list]
#'   List of \code{ecr_terminator} objects, which can be passed to \code{ecr}.
#' @export
#FIXME: this is not very flexible. What if we add new stopping conditions. In
# this case we have to change this function every time.
# The stopping condition generators should have attributes 'checkFunction' and
# default value (see ecr operators). This way setupTerminator could
# iterate over all available stopping conditions.
setupTerminators = function(max.iter = NULL, max.time = NULL) {
  if (is.null(max.iter) && is.null(max.time)) {
    stopf("At least max.iter or max.time must be finite.")
  }
  list(
    setupMaximumIterationsTerminator(max.iter),
    setupMaximumTimeTerminator(max.time)
  )
}
