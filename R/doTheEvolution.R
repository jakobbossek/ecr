#' @title
#' Working horse of the ecr package.
#'
#' @description
#' Takes a function and searches for a global optimum with an evolutionary approach.
#'
#' @keywords optimize
#'
#' @template arg_optimization_task
#' @template arg_control
#' @template arg_initial_population
#' @template arg_more_args
#' @return [\code{\link{ecr_result}}]
#'
#' @example examples/ex_doTheEvolution.R
#' @seealso \code{\link{setupECRControl}}
#' @export
doTheEvolution = function(task, control, initial.population = NULL, more.args = list()) {
  UseMethod("doTheEvolution")
}

#' @export
doTheEvolution.smoof_function = function(task, control, initial.population = NULL, more.args = list()) {
  task = makeOptimizationTask(task)
  doTheEvolution(task, control, initial.population)
}

#' @export
doTheEvolution.ecr_optimization_task = function(task, control, initial.population = NULL, more.args = list()) {
  doFinalChecks(task, control, more.args)
  task$more.args = more.args

  population = buildInitialPopulation(control$n.population, task, control, initial.population)
  population$fitness = evaluateFitness(population, task$fitness.fun, task, control)
  opt.state = setupOptState(task, population, control)
  fireEvent("onEAInitialized", control, opt.state)

  repeat {
    mating.pool = selectForMating(opt.state, control)
    fireEvent("onMatingPoolGenerated", control, opt.state)

    offspring = generateOffspring(opt.state, mating.pool, control)
    offspring$fitness = evaluateFitness(offspring, task$fitness.fun, task, control)
    fireEvent("onOffspringGenerated", control, opt.state)

    population = getNextGeneration(opt.state, offspring, control)
    updateOptState(opt.state, population, control)
    fireEvent("onPopulationUpdated", control, opt.state)

    stop.object = doTerminate(opt.state, control)
    if (length(stop.object) > 0L) {
      break
    }
  }
  fireEvent("onEAFinished", control, opt.state)

  return(setupResult(opt.state, stop.object, control))
}

# @title
# Do some final checks before EA initialization.
#
# @param task [ecr_optimization_task]
#   Optimization task.
# @param control [ecr_control]
#   Control object.
# @param more.args [list]
#   Additional arguments for objective function.
doFinalChecks = function(task, control, more.args) {
  assertClass(task, "ecr_optimization_task")
  assertClass(control, "ecr_control")
  assertList(more.args)

  if (isSmoofFunction(task$fitness.fun) && control$representation == "custom") {
    stopf("Custom representations not possible for smoof functions.")
  }

  if (control$representation == "float" && !hasFiniteBoxConstraints(task$par.set)) {
    stopf("Lower and upper box constraints needed for representation type 'float'.")
  }

  # check compatibility of selectors and #objectives
  checkSelectorCompatibility(task, control, control$parent.selector, control$survival.selector)
}

# @title
# Check selectors for compatibility with objectives.
#
# @param task [ecr_optimization_task]
#   Optimization task.
# @param control [ecr_control]
#   Control object.
# @param ... [any]
#   List of ecr_selector objects.
# @return Nothing
checkSelectorCompatibility = function(task, control, ...) {
  selectors = list(...)
  desired.obj = if (task$n.objectives == 1L) "single-objective" else "multi-objective"
  lapply(selectors, function(selector) {
    if (desired.obj %nin% attr(selector, "supported.objectives")) {
      stopf("Selector '%s' cannot be applied to problem with %i objectives.",
        getOperatorName(selector), task$n.objectives)
    }
  })
}

#' @title
#' Helper function to build initial population.
#'
#' @description
#' Generates the initial population of an EA based on a list of initial individuals
#' and/or the control object and the corresponding population generator.
#'
#' @param n.population [\code{integer(1)}]\cr
#'   Size of the population.
#' @param task [\code{ecr_optimization_task}]\cr
#'   Optimization task.
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @param initial.population [\code{list} | \code{NULL}]\cr
#'   Eventually a list of initial individuals.
#' @return [\code{ecr_population}]
#' @export
buildInitialPopulation = function(n.population, task, control, initial.population = NULL) {
  n.to.generate = n.population
  n.initial = 0L
  if (!is.null(initial.population)) {
    assertList(initial.population)
    n.initial = length(initial.population)
    if (n.initial > n.population) {
      stopf("Size of initial population (=%i) exceeds the specified population size %i.",
        n.initial, n.population)
    } else if (n.initial == n.population) {
      return(makePopulation(initial.population))
    }
  }
  populationGenerator = control$generator
  generated.population = populationGenerator(n.population - n.initial, task, control)
  if (n.initial > 0L) {
    return(makePopulation(c(generated.population$individuals, initial.population)))
  }
  return(generated.population)
}
