#' Factory method for monitor objects.
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
#' @export
makeMonitor = function(before = NULL, step = NULL, after = NULL, ...) {
  if (!is.null(before)) checkArg(before, cl = "function", na.ok = TRUE)
  if (!is.null(step)) checkArg(step, cl = "function", na.ok = TRUE)
  if (!is.null(after)) checkArg(after, cl = "function", na.ok = TRUE)
  dummy = function(...) {}
  structure(
    list(
      before = fallThrough(before, dummy),
      step = fallThrough(step, dummy),
      after = fallThrough(after, dummy)
    ),
    class = "ecr_monitor")
}
