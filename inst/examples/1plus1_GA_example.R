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

n.params = 25L

# now we wrap the objective function with the smoof package
obj.fun = makeSingleObjectiveFunction(
  fn = obj.fun,
  par.set = makeParamSet(
    makeIntegerVectorParam(len = n.params, id = "x", lower = 0, upper = 1)
  ),
  name = "ONE-MAX"
)

# Here we make use of mutations only! The nullRecombinator
# does nothing.
control = setupECRControl(
  n.population = 1L,
  n.offspring = 1L,
  n.mating.pool = 1L,
  representation = "binary",
  survival.strategy = "plus",
  stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 250L))
)
control = setupEvolutionaryOperators(
  control,
  generator = makeBinaryGenerator(),
  mutator = makeBitFlipMutator(mutator.flip.prob = 1 / n.params),
  recombinator = makeCrossoverRecombinator(),
)
print(control)

# actually apply the evolutionary algorithm
res = doTheEvolution(obj.fun, control = control)
print(res)

# plot optimization trace
print(autoplot(res, log.fitness = FALSE, complete.trace = TRUE))
