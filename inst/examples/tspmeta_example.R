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
n.nodes = 50L
inst = random_instance(size = n.nodes)

# The target fun is the length of a given tour
obj.fun = function(tour) {
  tour_length(x = inst, order = as.integer(tour))
}

# now we wrap the objective function with the smoof package
par.set = makeNumericParamSet(len = n.nodes, id = "c", lower = 1, upper = n.nodes)
obj.fun = makeSingleObjectiveFunction(fn = obj.fun, par.set = par.set, name = "Tour")

# Here we make use of mutations only! The nullRecombinator
# does nothing.
control = setupECRControl(
  n.population = 1000L,
  n.offspring = 10L,
  representation = "permutation",
  survival.strategy = "plus",
  n.elite = 1L,
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 2000L))
)
# here we stick to the defaults
control = setupEvolutionaryOperators(control)
print(control)

res = doTheEvolution(obj.fun, control = control)
print(res)

# plot computed tour
print(autoplot(inst, opt_tour = res$best.param))
