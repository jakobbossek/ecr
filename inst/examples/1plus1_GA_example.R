# Here we use the ecr package to optimize the ONE-MAX function,
# i. e., a simple binary function. ONE-MAX(x) = x_1 + ... + x_n,
# where n is the number of parameters and x_i in {0,1} for i = 1,...,n.
# We apply a very simple evolutionary algorithm, namely the (1+1)-GA
# to solve the problem and plot the optimization trace.
library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

set.seed(123)

# the target function is the popular ONE-MAX function
obj.fun = function(x) {
  # we want to maximize the number of 1's in the bitstring
  length(x) - sum(x)
}

n.params = 50L

# now we wrap the objective function with the otf package
obj.fun = makeSingleObjectiveFunction(
  fn = obj.fun,
  par.set = makeParamSet(
    makeIntegerVectorParam(len = n.params, id = "x", lower = 0, upper = 1)
  ),
  name = "ONE-MAX"
)

# Here we make use of mutations only! The nullRecombinator
# does nothing.
control = ecr.control(
  population.size = 1L,
  offspring.size = 1L,
  mating.pool.size = 1L,
  representation = "binary",
  survival.strategy = "plus",
  n.params = n.params,
  generator = makeBinaryGenerator(),
  mutator = list(bitflipMutator),
  recombinator = crossoverRecombinator,
  # see the literature on 1+1 GA for this parameter recommendation
  mutator.control = list(mutator.flip.prob = 1 / n.params),
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 500L))
)
print(control)

# actually apply the evolutionary algorithm
res = ecr(obj.fun, control = control)
print(res)

# plot optimization trace
print(autoplot(res, log.fitness = FALSE))
