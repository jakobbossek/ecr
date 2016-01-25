# @title
#   Implementation of the SPEA2 EMOA algorithm.
#
# @description
#   ...
#
# @note
#   This is a pure R implementation of the SPEA2 algorithm. It hides the regular
#   \pkg{ecr} interface and offers a more R like interface while still being quite
#   adaptable.
#
# @keywords optimize
#
# @references
#   ...
#
# @template arg_optimization_task
# @param n.population [\code{integer(1)}]\cr
#   Population size. Default is \code{100}.
# @param n.archive [\code{integer(1)}]\cr
#   Size of the pareto archive.
# @template arg_parent_selector
# @template arg_mutator
# @template arg_recombinator
# @template arg_max_iter
# @template arg_max_evals
# @template arg_max_time
# @return [\code{ecr_ecr_multi_objective_result}]
spea2 = function(
  task,
  n.population = 100L,
  n.archive,
  parent.selector = setupSimpleSelector(),
  mutator = setupGaussMutator(),
  recombinator = setupCrossoverRecombinator(),
  max.iter = 100L,
  max.evals = NULL,
  max.time = NULL) {

  if (isSmoofFunction(task)) {
    task = makeOptimizationTask(task)
  }
  assertInt(n.archive, na.ok = FALSE, lower = 2L)

  # Implementation of surival selection operator of the AS-EMOA algorithm.
  spea2Selector = makeSelector(
    selector = function(population, storage, task, n.select, control) {
      fitness = population$fitness
      population = population$individuals

      return(makePopulation(...))
    },
    supported.objectives = "multi-objective",
    name = "SPEA2 selector",
    description = "Selection takes place on modified fitness function."
  )

  # SPEA2 control object
  ctrl = setupECRControl(
    n.population = n.population,
    n.offspring = 1L,
    representation = "float",
    survival.strategy = "comma",
    monitor = setupConsoleMonitor(),
    stopping.conditions = list(
      setupMaximumEvaluationsTerminator(max.evals),
      setupMaximumTimeTerminator(max.time),
      setupMaximumIterationsTerminator(max.iter)
    )
  )

  ctrl = setupEvolutionaryOperators(
    ctrl,
    parent.selector = parent.selector,
    recombinator = recombinator,
    generator = setupUniformGenerator,
    mutator = mutator,
    survival.selector = asemoaSelector
  )

  ctrl$n.archive = n.archive
  ctrl$archive = list()

  return(doTheEvolution(task, ctrl))
}
