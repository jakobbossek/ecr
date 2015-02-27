# Here we use the ecr package to optimize the TSP tour for a
# random TSP instance with n = 20 cities. For sure there are
# better solvers for this optimization problem, but here it
# serves to demonstrate the packages ability to operate on
# permutations, since a TSP tour is a permutation of the cities.
library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(tspmeta)
library(ggplot2)

load_all(".")

set.seed(352)

# generate instance
n.nodes = 20L
inst = random_instance(size = n.nodes)

# The target fun is the length of a given tour
obj.fun = function(tour) {
  tour_length(x = inst, order = as.integer(tour))
}

# now we wrap the objective function with the smoof package
par.set = makeNumericParamSet(len = n.nodes, id = "c", lower = 1, upper = n.nodes, vector = FALSE)
obj.fun = makeSingleObjectiveFunction(fn = obj.fun, par.set = par.set, name = "Tour")

# Here we make use of mutations only! The nullRecombinator
# does nothing.
control = ecr.control(
  population.size = 100L,
  offspring.size = 50L,
  representation = "permutation",
  survival.strategy = "plus",
  elite.size = 1L,
  n.params = tspmeta:::number_of_cities(inst),
  generator = makePermutationGenerator(),
  mutator = list(swapMutator),
  recombinator = nullRecombinator,
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 200L))
)
print(control)

res = ecr(obj.fun, control = control)
print(res)

# plot computed tour
print(autoplot(inst, opt_tour = res$best.param))
