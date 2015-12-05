#' @title Simple stdout monitoring function.
#'
#' @description
#' This is the default monitoring function used by ecr. It simply outputs
#' the iteration as well as minimal, mean and maximal target values from the current
#' population.
#'
#' @param show.info.stepsize [\code{integer(1)}]\cr
#'   Adjust the stepsize of iterations with informative messages.
#' @param num.format [\code{character(1)}]\cr
#'   Number format for output of numeric parameters. See the details section of
#'   the manual for \code{\link[base]{sprintf}} for details.
#'   Default is \dQuote{\%g}.
#' @return [\code{ecr_monitor}]
#' @export
makeConsoleMonitor = function(show.info.stepsize = 5L, num.format = "%g") {
  assertInteger(show.info.stepsize, len = 1L, lower = 1L, upper = 100L, any.missing = FALSE)
  assertString(num.format)

  force(show.info.stepsize)
  force(num.format)

  makeMonitor(
    before = function(envir = parent.frame()) {
      cat("Initialization finished! Starting optimization process ...\n")
    },
    step = function(envir = parent.frame()) {
      max.iter = envir$control$max.iter
      fitness = envir$population$fitness
      iter = envir$iter
      if ((iter %% show.info.stepsize) == 0L) {
        call.format = sprintf("Iter %s | y (min: %s, mean: %s, max: %s)", "%i", num.format, num.format, num.format)
        catf(call.format, iter, min(fitness), mean(fitness), max(fitness))
      }
    },
    after = function(envir = parent.frame()) {
      cat("Finished!\n")
    }
  )
}
