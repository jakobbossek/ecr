#' Simple monitoring function.
#'
#' This is the default monitoring function used by ecr. It simply outputs
#' the iteration as well as minimal, mean and maximal target values from the current
#' population.
#'
#' @export
makeConsoleMonitor = function() {
  makeMonitor(
    before = function(...) cat("Initialization finished! Starting optimization process ...\n"),
    step = function(objective.fun, population, trace, iter, control) {
      max.iter = control$max.iter
      fitness = population$fitness
      if (control$n.targets == 1L)
        catf("Iter %i | y (min: %0.2g, mean: %0.2g, max: %0.2g)", iter, min(fitness), mean(fitness), max(fitness))
    },
    after = function(...) cat("Finished!\n")
  )
}
