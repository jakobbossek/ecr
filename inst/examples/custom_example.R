library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

set.seed(123)

getPairwiseDistances = function(x) {
  distances = c()
  n = nrow(x)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        next
      }
      d = sum(x[i, ] - x[j, ])^2
      distances = c(distances, d)
    }
  }
  return(distances)
}

# defs
N = 15L

#fitness = makeFitnessFunction(
fitness = function(x, ...) {
  dists = getPairwiseDistances(x)
  return(1 / sum(dists)) # since we want to maximise here
}

# control and operator settings are separated now
ctrl = setupECRControl(
  n.population = 100L,
  n.offspring = 10L,
  representation = "custom", # bypass everything
  survival.strategy = "plus",
  monitor = makeConsoleMonitor(),
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 10000L))
)

myGenerator = makeGenerator(
  generator = function(size, control) {
    makePopulation(lapply(seq(size), function(i) {
      matrix(runif(N * 2L), ncol = 2L)
    }))
  },
  name = "Point generator",
  description = "Generates random point clouds in the euclidean space",
  supported = "custom"
)

myMutator = makeMutator(
  mutator = function(x, control) {
    idx = which(runif(nrow(x)) < 0.1)
    x[idx, ] = matrix(runif(2 * length(idx)), ncol = 2)
    return(x)
  },
  name = "Point-Shift mutation",
  description = "Shift all points in a random direction",
  supported = "custom"
)

myRecombinator = makeRecombinator(
  recombinator = function(x) {
    x[[1]]
  },
  name = "Convex-Combination recombinator",
  description = "Make a convex combination of the point coordinates",
  supported = "custom"
)

ctrl = setupEvolutionaryOperators(
  ctrl,
  generator = myGenerator,
  mutator = myMutator,
  recombinator = myRecombinator
)

res = doTheEvolution(fitness, ctrl)
plot(res$best.param)
