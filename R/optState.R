#' @title
#' Optimization state.
#'
#' @description
#' Description is coming soon ...
#'
#' @name OptState
#' @rdname OptState
NULL


# @title Update best individual.
#
# @description Select the best individual of the current population and eventually
# replace the best so far.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
updateOptStateBestIndividual = function(opt.state) {
  task = opt.state$task

  population.best = getBestIndividual(opt.state)
  #FIXME: ugly as sin! Redundant code
  if (task$minimize) {
    if (population.best$fitness <= opt.state$best.value) {
      opt.state$best.param = population.best$individual
      opt.state$best.value = population.best$fitness
    }
  } else {
    if (population.best$fitness >= opt.state$best.value) {
      opt.state$best.param = population.best$individual
      opt.state$best.value = population.best$fitness
    }
  }
  invisible()
}
