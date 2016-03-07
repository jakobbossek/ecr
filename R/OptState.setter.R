#' @title Generate an optimization state object.
#'
#' @description
#' The optimization task object is a container for all variables
#' regarding the optimization process.
#'
#' @param task [\code{ecr_optimization_task}]\cr
#'   Optimization task.
#' @param population [\code{ecr_population}]\cr
#'   (Initial) population.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @return [\code{ecr_opt_state}]
#' @export
setupOptState = function(task, population, control) {
  opt.state = new.env()
  opt.state$iter = 0L
  opt.state$time.created = Sys.time()
  opt.state$time.passed = 0
  opt.state$task = task
  opt.state$par.set = task$par.set
  opt.state$n.evals = control$n.population
  opt.state$control = control

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    best = getBestIndividual(population, task)
    opt.state$best.param = best$individual
    opt.state$best.value = best$fitness
  }

  # construct opt path
  y.names = paste0("y", seq(task$n.objectives))

  # EA specific
  opt.state$population = population
  obj.class = paste("ecr", ifelse (task$n.objectives == 1L, "single_objective", "multi_objective"), "opt_state", sep = "_")
  class(opt.state) = c("ecr_opt_state", obj.class, class(opt.state))
  return(opt.state)
}

#' @title Update optimization state.
#'
#' @description
#' Called once after survival selection to update the optimization state.
#'
#' @param opt.state [\code{ecr_opt_state}]\cr
#'   Optimization state.
#' @param population [\code{ecr_population}]\cr
#'   Current population.
#' @param control [\code{ecr_control}]\cr.
#'   Control object.
#' @return Nothing.
#' @export
updateOptState = function(opt.state, population, control) {
  task = opt.state$task

  # update stuff
  opt.state$iter = opt.state$iter + 1L
  opt.state$n.evals = opt.state$n.evals + control$n.offspring
  opt.state$time.passed = difftime(Sys.time(), opt.state$time.created, units = "secs")

  # update population
  opt.state$population = population

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    updateOptStateBestIndividual(opt.state)
  }

  invisible()
}
