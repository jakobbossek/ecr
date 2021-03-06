# Here we use to optimise a function with two vectors of bitstrings.
# The optimum of the function:
# the first vector contains only ones,
# the second vector contains only zeros.
library(devtools)
library(BBmisc)

load_all(".", reset = TRUE)

# objective function
obj.fn = makeSingleObjectiveFunction(
  name = "Number of Ones",
  fn = function(x1, x2) {
    (length(x1) - sum(x1)) + sum(x2)
  },
  par.set = makeParamSet(
    makeIntegerVectorParam("x1", len = 20L, lower = 0, upper = 1),
    makeIntegerVectorParam("x2", len = 15L, lower = 0, upper = 1)
  ),
  has.simple.signature = FALSE
)

makeOptimumAppearsTerminator = function(opt.fitness = 0) {
  condition.fun = function(opt.state) {
    min.fitness = min(opt.state$population$fitness)
    return(min.fitness == 0)
  }
  makeTerminator(
    condition.fun,
    name = "OptimumAppeared",
    message = sprintf("Optimum appeared in population.")
  )
}

control = setupECRControl(
  n.population = 100L,
  n.offspring = 100L,
  n.mating.pool = 100L,
  representation = "binary",
  logger = setupOptPathLoggingMonitor(step = 10L),
  monitor = setupConsoleMonitor(1L),
  stopping.conditions = list(
    makeOptimumAppearsTerminator()
  )
)

control = setupEvolutionaryOperators(
  control,
  parent.selector = setupRouletteWheelSelector(),
  generator = setupBinaryGenerator(),
  mutator = setupBitFlipMutator(p = 0.001),
  recombinator = setupCrossoverRecombinator(p = 0.7)
)

# Show all column names in opt.path:
# names(as.data.frame(res$opt.path))
res = doTheEvolution(obj.fn, control = control)
print(res)
autoplot(res, complete.trace = TRUE)
