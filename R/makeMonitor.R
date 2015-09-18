#' @title
#'   Factory method for monitor objects.
#'
#' @description
#'   Monitor objects serve for monitoring the optimization process. Each monitor
#'   object expects the parameters \code{before}, \code{step} and \code{after},
#'   each being a function and expecting \code{envir = parent.frame()} as the
#'   only parameter. This way one can access all the variables used within the
#'   evolutionary cycle.
#'
#' @param before [\code{function}]\cr
#'   Function called one time after initialization of the EA.
#' @param step [\code{function}]\cr
#'   Function applied after each iteration of the algorithm.
#' @param after [\code{function}]\cr
#'   Function applied after the EA terminated.
#' @param ... [\code{any}]\cr
#'   Not used.
#' @return [\code{ecr_monitor}]
#'   Monitor object.
#'
#' @example examples/ex_makeMonitor.R
#' @export
makeMonitor = function(before = NULL, step = NULL, after = NULL, ...) {
  if (!is.null(before)) assertFunction(before)
  if (!is.null(step)) assertFunction(step)
  if (!is.null(after)) assertFunction(after)
  dummy = function(...) {}
  structure(
    list(
      before = coalesce(before, dummy),
      step = coalesce(step, dummy),
      after = coalesce(after, dummy)
    ),
    class = "ecr_monitor")
}
