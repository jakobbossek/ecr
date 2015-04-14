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

#fitness = makeFitnessFunction(
fitness = function(x, ...) {
    dists = getPairwiseDistances(x)
    return(1 / sum(dists)) # since we want to maximise here
  }
#

# control and operator settings are separated now
ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 10L,
  representation = "custom", # bypass everything
  survival.strategy = "plus",
  monitor = makeConsoleMonitor()
)

myGenerator = makeGenerator(
  generator = function(size, control) {
    matrix(runif(N * 2L), ncol = 2L)
  },
  name = "Point generator",
  description = "Generates random point clouds in the euclidean space",
  supported = "custom"
})

myMutator = makeMutator(
  mutator = function(x) {
    N = nrow(x)
    x + runif(N * 2L, ncol = 2L, min = 0, max = 0.01)
  },
  name = "Point-Shift mutation",
  description = "Shift all points in a random direction",
  supported = "custom"
)

myRecombinator = makeRecombinator(
  recombinator = function(x, y) {
    N = nrow(x)
    0.5 * (x + y)
  },
  name = "Convex-Combination recombinator",
  description = "Make a convex combination of the point coordinates",
  supported = "custom"
)

control = setupEvolutionaryOperators(
  control,
  generator = myGenerator,
  mutator = myMutator,
  recombinator = myRecombinator
)

res = doTheEvolution(fitness, ctrl)
