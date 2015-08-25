# @title
#   Generator for trace objects.
#
# @description
#   Trace objects are kind of a wrapper for optPath and other stuff which is specific
#   to single- and multiobjective tracing respectively.
#
# @param control [ecr_control]
#   Control object.
# @param population [ecr_population]
#   Current population.
# @param n.objecjtives [integer(1)]
#   Number of targets/objectives.
# @param y.names [character(1)]
#   Names for the y-columns in the opt.path.
# @return [ecr_single_objective_trace | ecr_multi_objective_trace]
initTrace = function(control, population, task) {
  par.set = task$par.set
  y.names = paste0("y", seq(task$n.objectives))
  opt.path = makeOptPathDF(par.set, y.names = y.names, minimize = task$minimize,
    include.extra = TRUE, include.exec.time = TRUE
  )

  if (task$n.objectives == 1L) {
    best = getBestIndividual(population)
    return(makeS3Obj(
      opt.path = opt.path,
      best = best,
      classes = c("ecr_single_objective_trace")
    ))
  }
  return(makeS3Obj(
    opt.path = opt.path,
    classes = c("ecr_multi_objective_trace")
  ))
}

# @title
#   Updates a trace with the information of the current population.
#
# @param trace [ecr_{single,multi}_objective_trace]
#   Trace to update.
# @param iter [integer(1)]
#   Current iteration/generation.
# @param n.evals [integer(1)]
#   Number of function evaluations.
# @param population [ecr_population]
#   Population object.
# @param start.time [POSIXct]
#   Beginning of the optimization process.
# @param exec.time [numeric(1)]
#   Time it took to generate the initial population or set up the new population
#   respectively.
# @param control [ecr_control]
#   Control object.
# @return [ecr_{single,multi}_objective_trace] Modified trace.
updateTrace = function(trace, iter, n.evals, population, start.time, exec.time, control) {
  UseMethod("updateTrace")
}

# see generic updateTrace
updateTrace.ecr_single_objective_trace = function(trace, iter, n.evals, population, start.time, exec.time, control) {
  par.set = control$par.set
  best = getBestIndividual(population)
  extras = getListOfExtras(iter, n.evals, population, start.time, control)
  if (length(par.set$pars) == 1L) {
    best.param.values = list(best$individual)
    names(best.param.values) = getParamIds(par.set)
  } else {
    best.param.values = as.list(best$individual)
    names(best.param.values) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  }
  #FIXME: dummy value for custom representation
  if (control$representation == "custom") {
    best.param.values = list("x" = 0.5)
  }
  addOptPathEl(trace$opt.path, x = best.param.values, y = unlist(best$fitness), dob = iter,
    exec.time = exec.time, extra = extras, check.feasible = FALSE)
  trace$best = best
  return(trace)
}

# see generic updateTrace
updateTrace.ecr_multi_objective_trace = function(trace, iter, n.evals, population, start.time, exec.time, control) {
  par.set = control$par.set
  extras = getListOfExtras(iter, n.evals, population, start.time, control)
  #FIXME: handle this specific stuff here.
  if (control$representation == "custom") {
    stopf("Multi-objective optimization with custom genotypes is not yet finished.")
    best.param.values = list("x" = 0.5)
  }
  n.population = length(population$individuals)
  #FIXME: since we store the entire population anew, we set the eol stuff
  # if (iter > 1L) {
  #   dobs = getOptPathDOB(trace$opt.path)
  #   idx = which(dobs == (iter - 1L))
  #   setOptPathElEOL(trace$opt.path, index = idx, eol = iter)
  # }

  for (i in seq(n.population)) {
    x = as.list(population$individuals[[i]])
    names(x) = getParamIds(control$par.set, with.nr = TRUE, repeated = TRUE)
    addOptPathEl(trace$opt.path, x = x, y = population$fitness[, i], dob = iter,
    exec.time = 0, extra = extras, check.feasible = FALSE)
  }
  return(trace)
}
