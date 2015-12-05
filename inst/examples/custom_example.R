library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

set.seed(123)

# defs
N = 15L

#fitness = makeFitnessFunction(
fitness = function(x, ...) {
  dists = dist(x)
  return(1 / sum(dists)) # since we want to maximise here
}

# control and operator settings are separated now
ctrl = setupECRControl(
  n.population = 100L,
  n.offspring = 10L,
  representation = "custom", # bypass everything
  survival.strategy = "plus",
  monitor = makeConsoleMonitor(),
  custom.constants = list(N = N),
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 1000L))
)

myGenerator = makeGenerator(
  generator = function(size, task, control) {
    makePopulation(lapply(seq(size), function(i) {
      matrix(runif(control$custom.constants$N * 2L), ncol = 2L)
    }))
  },
  name = "Point generator",
  description = "Generates random point clouds in the euclidean space",
  supported = "custom"
)

myMutator = makeMutator(
  mutator = function(ind, task, control) {
    idx = which(runif(nrow(ind)) < 0.1)
    ind[idx, ] = matrix(runif(2 * length(idx)), ncol = 2)
    return(ind)
  },
  name = "Point-Shift mutation",
  description = "Shift all points in a random direction",
  supported = "custom"
)

myRecombinator = makeRecombinator(
  recombinator = function(inds, task, control) {
    inds[[1]]
  },
  name = "Convex-Combination recombinator",
  description = "Make a convex combination of the point coordinates",
  supported = "custom"
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  parent.selector = makeTournamentSelector(),
  generator = myGenerator,
  mutator = myMutator,
  recombinator = myRecombinator,
  survival.selector = makeGreedySelector()
)

res = doTheEvolution(makeOptimizationTask(fitness, n.objectives = 1L), ctrl)
plot(res$best.param)
