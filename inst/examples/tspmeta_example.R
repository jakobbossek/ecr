# Here we use the ecr package to optimize the TSP tour for a
# random TSP instance with n = 20 cities. For sure there are
# better solvers for this optimization problem, but here it
# serves to demonstrate the packages ability to operate on
# permutations, since a TSP tour is a permutation of the cities.
library(methods)
library(testthat)
library(devtools)
library(soobench)
library(BBmisc)
library(tspmeta)

load_all(".")

set.seed(352)
inst = random_instance(size = 10L)

# The target fun is the length of a given tour
obj.fun = function(tour) {
  tour_length(x = inst, order = as.integer(tour))
}

par.set = makeNumericParamSet(len = 10, id = "c", lower = 1, upper = 10, vector = FALSE)

# Here we make use of mutations only! The nullRecombinator
# does nothing.
control = ecr.control(
  population.size = 10L,
  offspring.size = 50L,
  representation = "permutation",
  survival.strategy = "comma",
  elite.size = 1L,
  max.iter = 30L,
  n.params = tspmeta:::number_of_cities(inst),
  n.targets = 1L,
  generator = makePermutationGenerator(),
  mutator = list(swapMutator),
  recombinator = nullRecombinator
)
print(control)

res = ecr(obj.fun, par.set = par.set, control = control)
