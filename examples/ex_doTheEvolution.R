library(smoof)
library(ParamHelpers)
library(ggplot2)

# We want to find the minimum of the function f(x) = x sin(2x) on the interval
# [0, 2pi]. The optimal value is about -5.5 for x = 5.54.
# First we wrap the function with the smoof package:
obj.fn = makeSingleObjectiveFunction(
  name = "My obj. function",
  fn = function(x) x * sin(2 * x),
  par.set = makeParamSet(makeNumericParam("x", lower = 0, upper = 2 * pi))
)

# We want to solve this with a (10 + 10) evolutionary strategy based on
# the floating point representation of the input vectors with the default
# operators: intermediate recombinator and Gaussian mutation
ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 10L,
  survival.strategy = "plus",
  representation = "float",
  stopping.conditions = setupStoppingConditions(max.iter = 100L)
)
# use the default operators for representation "float"
ctrl = setupEvolutionaryOperators(ctrl)

res = doTheEvolution(obj.fn, ctrl)
print(res)

# Now let us choose a (10, 10) strategy with intermediate recombination. Moreover,
# we want the "fittest" individual to surive each time and therefore set n.elite
# to 1.
ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 10L,
  survival.strategy = "comma",
  n.elite = 1L,
  representation = "float",
  stopping.conditions = setupStoppingConditions(max.iter = 100L)
)
ctrl = setupEvolutionaryOperators(ctrl)

res = doTheEvolution(obj.fn, ctrl)
print(res)

# Now let us tackle a two-dimensional problem
# Here we aim to find the global optimum of the 2D-Rastrigin function

set.seed(1234)

obj.fn = makeRastriginFunction(2L)
autoplot(obj.fn, show.optimum = TRUE)
# The global optimum is located in x1 = x2 = 0 with a function value of 0.

# We choose a (100 + 10) strategy with crossover recombination here and save
# each population. This way we can visualize the population later on.
ctrl = setupECRControl(
  n.population = 100L,
  n.offspring = 10L,
  survival.strategy = "plus",
  save.population.at = 0:100L,
  representation = "float",
  stopping.conditions = setupStoppingConditions(max.iter = 100L),
)
ctrl = setupEvolutionaryOperators(
 ctrl,
 selector = makeRouletteWheelSelector(),
 recombinator = makeCrossoverRecombinator()
)
res = doTheEvolution(obj.fn, ctrl)
print(res)
\dontrun{
  autoplot(res, show.process = TRUE)
}
