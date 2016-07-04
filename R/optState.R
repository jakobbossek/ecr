#' @title
#' Optimization state.
#'
#' @description
#' The optimization state of class \code{ecr_opt_state} is a S3 object containing
#' all the information about the current state of the optimization process. It
#' is initialized uopn the generation of the initial population right before the
#' evolutionary loop is started and is updated after after a new generation was
#' created, i.~e., after each iteration of the EA loop. Internally the object
#' is an R environment which allows for manipulation in place. By default the
#' state object contains the following slots:
#' \describe{
#'   \item{iter}{The current iteration/generation.}
#'   \item{time.created}{Time the object was initialized.}
#'   \item{time.passed}{Time passed since creation.}
#'   \item{task}{The optimization task (see \code{\link{makeOptimizationTask}}).}
#'   \item{pas.set}{The parameter set of the objective function at hand (see \code{\link[ParamHelpers]{ParamSet}}).}
#'   \item{n.evals}{Number of performed objective function evaluations.}
#'   \item{control}{The \code{ecr_control} control object.}
#'   \item{population}{The current population.}
#'   \item{best.param/best.value}{Best parameter and its value (only in single-objective case).}
#' }
#' The final optimization state object is returned by the EA (see \code{\link{ecr_result}}).
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
