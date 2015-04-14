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
#' @examples
#' # We want to find the minimum of the function f(x) = x sin(2x) on the intervall
#' # [0, 2pi]. First we wrap the function with the smoof package:
#'
#' # We optimize the function f(x) = x sin(2x) here with a simple (10 + 10)
#' # evolutionary strategy. We overwrite the default console monitor
#' # (see function makeConsoleMonitor) with an enhanced console monitor :-)
#' obj.fn = makeSingleObjectiveFunction(
#'   name = "My obj. function",
#'   fn = function(x) x * sin(2 * x),
#'   par.set = makeParamSet(makeNumericParam("x", lower = 0, upper = 2 * pi))
#' )
#' # Now we define our enhanced monitoring function
#' # Monitor functions expect the parent environment as the only parameter
#' # This way we can access all the variables saved there.
#' monitorStep = function(envir = parent.frame()) {
#'   iter = envir$iter
#'   best.fitness = envir$best$fitness
#'   if (iter == 1L) {
#'     envir$first.best = best.fitness
#'   }
#'   first.best.fitness = envir$first.best
#'   cat(sprintf("Best objective value in iteration %i is %.6f
#'     (overall absolute improvement is: %.6f)\n",
#'     iter, best.fitness, first.best.fitness - best.fitness)
#'   )
#' }
#' myFancyConsoleMonitor = makeMonitor(
#'   before = function(envir = parent.frame()) {
#'     catf("I am starting now buddy!")
#'   },
#'   step = monitorStep,
#'   after = function(envir = parent.frame()) {
#'     catf("Finished!")
#'   }
#' )
#' # We want to solve this with a (10 + 10) evolutionary strategy based on
#' # the floating point representation of the input vectors with the default
#' # operators: intermediate recombinator and Gauss mutation
#' ctrl = setupECRControl(
#'   n.population = 10L,
#'   n.offspring = 10L,
#'   survival.strategy = "plus",
#'   n.params = 1L,
#'   representation = "float",
#'   stopping.conditions = setupStoppingConditions(max.iter = 30L),
#'   monitor = myFancyConsoleMonitor
#' )
#' res = doTheEvolution(obj.fn, ctrl)
#' print(res)
#'
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
