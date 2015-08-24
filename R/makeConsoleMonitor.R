#' @title
#'   Simple stdout monitoring function.
#'
#' @description
#'   This is the default monitoring function used by ecr. It simply outputs
#'   the iteration as well as minimal, mean and maximal target values from the current
#'   population.
#'
#' @param show.info.stepsize [\code{integer(1)}]\cr
#'   Adjust the stepsize of iterations with informative messages.
#' @return [\code{ecr_monitor}]
#' @export
makeConsoleMonitor = function(show.info.stepsize = 5L) {
  force(show.info.stepsize)
  assertInteger(show.info.stepsize, len = 1L, lower = 1L, upper = 100L, any.missing = FALSE)

  makeMonitor(
    before = function(envir = parent.frame()) {
      cat("Initialization finished! Starting optimization process ...\n")
    },
    step = function(envir = parent.frame()) {
      max.iter = envir$control$max.iter
      fitness = envir$population$fitness
      iter = envir$iter
      if ((iter %% show.info.stepsize) == 0L) {
        catf("Iter %i | y (min: %0.2g, mean: %0.2g, max: %0.2g)",
          iter, min(fitness), mean(fitness), max(fitness))
      }
    },
    after = function(envir = parent.frame()) {
      cat("Finished!\n")
    }
  )
}
