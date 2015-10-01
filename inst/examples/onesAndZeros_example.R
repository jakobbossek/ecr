# Here we use to optimise a function with two vectors of bitstrings.
# The optimum of the function:
# the first vector contains only ones,
# the second vector contains only zeros.

# clear all
rm(list = ls(all = TRUE))
library(devtools)
library(BBmisc)
# load ecr
load_all(".", reset = TRUE)

# objective function
obj.fn = makeSingleObjectiveFunction(
  name = "Number of Ones"
  , fn = function(x, y) {
    (length(x) - sum(x)) + sum(y)
  }
  , par.set = makeParamSet(
    makeIntegerVectorParam("x", len = 20L, lower = 0, upper = 1)
    , makeIntegerVectorParam("y", len = 15L, lower = 0, upper = 1)
  )
)
makeOptimumAppearsStoppingCondition = function(opt.fitness = 0) {
  condition.fun = function(opt.path) {
    # i.best = getOptPathBestIndex(opt.path)
    min.fitness.vector = getOptPathCol(opt.path, "pop.min.fitness")
    return(min(min.fitness.vector) == 0)
  }
  makeStoppingCondition(condition.fun
                        , name = "OptimumAppeared"
                        , message = sprintf("Optimum appeared in population.")
  )
}
control = setupECRControl(
  n.population = 100L
  , n.offspring = 100L
  , n.mating.pool = 100L
  , representation = "binary"
  , save.population.at = c(0L, 10L, 20L, 30L, 40L, 50L, 60L)
  , monitor = makeConsoleMonitor(1L)
  , stopping.conditions = list(
    # makeMaximumIterationsStoppingCondition(max.iter = 100L)
    makeOptimumAppearsStoppingCondition()
  )
)
control = setupEvolutionaryOperators(
  control
  , parent.selector = makeRouletteWheelSelector()
  , generator = makeBinaryGenerator()
  , mutator = makeBitFlipMutator(p = 0.001)
  , recombinator = makeCrossoverRecombinator(p = 0.7)
)

# Show all column names in opt.path:
# names(as.data.frame(res$opt.path))
res = doTheEvolution(obj.fn, control = control)
print(res)
autoplot(res, complete.trace = TRUE)
