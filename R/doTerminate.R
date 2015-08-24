#' @title
#'   Check termination conditions.
#'
#' @description
#'   Helper function which checks whether some stopping conditions
#'   are met and returns a termination object.
#'
#' @param stopping.funs [list]
#'   List of stopping conditions.
#' @param opt.path [OptPath]
#'   Optimization path.
#' @return [stopObject]
doTerminate = function(stopping.funs, opt.path) {
  stopObject = list()

    # if we have not specified any stopping conditions always return the empty object
  if (!length(stopping.funs)) {
    return(stopObject)
  }

    # otherwise iterate over stopping conditions and check
  for (stopping.fun in stopping.funs) {
    shouldStop = stopping.fun(opt.path = opt.path)
    if (shouldStop) {
      stopObject$name = attr(stopping.fun, "name")
      stopObject$message = attr(stopping.fun, "message")
      break
    }
  }
  return(stopObject)
}
