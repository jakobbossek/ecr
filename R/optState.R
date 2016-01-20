# @title Generate an optimization state object.
#
# @description The optimization task object is a container for all variables
# regarding the optimization process.
#
# @param task [\code{ecr_optimization_task}]\cr
#   Optimization task.
# @param population [\code{ecr_population}]\cr
#   (Initial) population.
# @param control [\code{ecr_control}]\cr
#   Control object.
# @return [\code{ecr_opt_state}]
setupOptState = function(task, population, control) {
  opt.state = new.env()
  opt.state$iter = 0L
  opt.state$time.created = Sys.time()
  opt.state$task = task
  opt.state$par.set = task$par.set
  opt.state$n.evals = control$n.population

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    best = getBestIndividual(population, task)
    opt.state$best.param = best$individual
    opt.state$best.value = best$fitness
  }

  # optional logging of population
  opt.state$population.storage = namedList(paste0("gen.", control$save.population.at))

  if (0 %in% control$save.population.at) {
    opt.state$population.storage[[paste0("gen.", as.character(0))]] = population
  }

  # construct opt path
  #FIXME: introduce logical use.opt.path control parameter
  #FIXME: y.names should be paraemter of task
  y.names = paste0("y", seq(task$n.objectives))
  opt.state$opt.path = makeOptPathDF(
    task$par.set, y.names = y.names, minimize = task$minimize,
    include.extra = TRUE, include.exec.time = TRUE
  )

  # EA specific
  opt.state$population = population
  obj.class = paste("ecr", ifelse (task$n.objectives == 1L, "single_objective", "multi_objective"), "opt_state", sep = "_")
  class(opt.state) = c("ecr_opt_state", obj.class, class(opt.state))
  return(opt.state)
}

# @title Update optimization state.
#
# @decscription Called once after survival selection to update the optimization
# state.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
# @param population [\code{ecr_population}]\cr
#   Current population.
# @param control [\code{ecr_control}]\cr.
#   Control object.
updateOptState = function(opt.state, population, control) {
  task = opt.state$task

  # update stuff
  opt.state$iter = opt.state$iter + 1L
  opt.state$n.evals = opt.state$n.evals + control$n.offspring
  opt.state$time.passed = difftime(Sys.time(), opt.state$time.created, units = "secs")

  # update population
  opt.state$population = population

  #FIXME/TODO: handle opt.path update!

  # save best-so-far solution in single-objective case
  if (task$n.objectives == 1L) {
    updateOptStateBestIndividual(opt.state)
  }

  # update populaton storage
  if (opt.state$iter %in% control$save.population.at) {
    opt.state$population.storage[[paste0("gen.", as.character(opt.state$iter))]] = population
  }

  invisible()
}

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
