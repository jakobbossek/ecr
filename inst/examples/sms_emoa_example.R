library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

set.seed(1)
load_all(".")

# THIS FILE IS WORK-IN-PROGRESS/EXPERIMENAL

# COLLECTION OF FIXME
#FIXME: integrate NondominatedSetSelector as ecr_selector
#FIXME: we need to check in setupECRControl or in ecr if the operators can work
# on multi-objective stuff


# Get set of dominated individuals.
#
# @param x [list]
#   Set of 2D fitness values.
# @param fn [function]
#   Fitness function.
# @return [list]
#FIXME: formulate this for matrices where each column describes one vector
getDominatedSet = function(x) {
  n = length(x)

  # initialize set of dominated individuals
  dom.set = integer()
  dom.nrs = integer(n)

  for (i in seq(n)) {
    for (j in seq(n)) {
      if (i == j) {
        next
      }
      if (all(x[[j]] <= x[[i]])) {
        dom.nrs[i] = dom.nrs[i] + 1L
      }
    }
  }
  return(list(
    dom.set = which(dom.nrs != 0L),
    dom.nrs = dom.nrs
  ))
}

makeNondominatedSetSelector = function() {
  makeSelector(
    selector = function(population, n.select, control) {
      inds = population$individuals
      n = length(inds)
      #FIXME: ugly as sin. See fixme of getNondominatedSet
      fitness = as.list(as.data.frame(population$fitness))
      res = getDominatedSet(fitness)
      idx = getMaxIndex(res$dom.nrs)
      survive.idx = setdiff(seq(n), idx)
      return(makePopulation(inds[survive.idx], population$fitness[, survive.idx, drop = FALSE]))
    },
    supported.objectives = "multi-objective",
    name = "Nondominated set selector",
    description = "description"
  )
}

ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 1L,
  representation = "float",
  monitor = makeConsoleMonitor(),
  stopping.conditions = setupStoppingConditions(max.iter = 100L)
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  parent.selector = makeSimpleSelector(),
  mutator = makeGaussMutator(),
  recombinator = makeCrossoverRecombinator(),
  survival.selector = makeNondominatedSetSelector()
)

obj.fn = smoof::makeZDT1Function(2L)

res = doTheEvolution(obj.fn, ctrl)

pl = visualizeParetoOptimalFront(obj.fn)
pf = as.data.frame(res$pareto.front)
pl = pl + geom_point(data = pf, aes(x = y1, y = y2), colour = "green")
stop("FIN")
