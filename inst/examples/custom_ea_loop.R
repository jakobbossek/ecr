library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(smoof)

load_all(".")

# the following example illustrates how to write an EA loop by hand with ecr
# helper functions. Even though doTheEvolution is quite powerful, not all tasks
# might be easily realizable.

# In this example we optimize the 2D sphere function with a custom loop.
fn = makeSphereFunction(2L)

# setup the control object (neccesary for custom loops as well)
ctrl = setupECRControl(
  n.population = 20L,
  n.offspring = 5L,
  representation = "float",
  survival.strategy = "plus",
  stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 100L))
)

# we need to create an optimization task by hand
task = makeOptimizationTask(fn)

# build initial population
population = buildInitialPopulation(ctrl$n.population, task, ctrl)

# evaluate fitness function
population$fitness = evaluateFitness(population, task$fitness.fun, task, ctrl)

# now generate optimization state
opt.state = setupOptState(task, population, ctrl)

# now start the evolutionary cycle
repeat {
  # first select some individuals for mating
  mating.pool = selectForMating(opt.state, ctrl)

  # then generate the offspring
  offspring = generateOffspring(opt.state, mating.pool, ctrl)
  offspring$fitness = evaluateFitness(offspring, task$fitness.fun, task, ctrl)

  # apply the survival selection
  population = getNextGeneration(opt.state, offspring, ctrl)

  # update the optimization state
  updateOptState(opt.state, population, ctrl)

  # check termination conditions
  stop.object = doTerminate(opt.state, ctrl)
  if (length(stop.object) > 0L) {
    break
  }
}

# generate result object
res = setupResult(opt.state, stop.object, ctrl)
