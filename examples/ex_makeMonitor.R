# We want to find the minimum of the function f(x) = x sin(2x) on the intervall
# [0, 2pi]. We optimize f here with a simple (10 + 10)
# evolutionary strategy. We overwrite the default console monitor
# (see function setupConsoleMonitor) with an enhanced console monitor :-)
obj.fn = makeSingleObjectiveFunction(
  name = "My obj. function",
  fn = function(x) x * sin(2 * x),
  par.set = makeParamSet(makeNumericParam("x", lower = 0, upper = 2 * pi))
)

# Now we define our enhanced monitoring function
# Monitor functions expect the opt.state (optimization state) and ... (not used
# until now). This way we can access all the variables saved there.
monitorStep = function(opt.state, ...) {
  iter = opt.state$iter
  best.fitness = opt.state$best.value
  if (iter == 1L) {
    # manupulate opt.state
    opt.state$first.best = best.fitness
  }
  first.best.fitness = opt.state$first.best
  cat(sprintf("Best objective value in iteration %i is %.6f
    (overall absolute improvement is: %.6f)\n",
    iter, best.fitness, first.best.fitness - best.fitness)
  )
}

myFancyConsoleMonitor = makeMonitor(
  before = function(opt.state, ...) {
    catf("I am starting now buddy!")
  },
  step = monitorStep,
  after = function(opt.state, ...) {
    catf("Finished!")
  }
)

# We want to solve this with a (10 + 10) evolutionary strategy based on
# the floating point representation of the input vectors with the default
# operators: intermediate recombinator and Gauss mutation
ctrl = setupECRControl(
  n.population = 10L,
  n.offspring = 10L,
  survival.strategy = "plus",
  representation = "float",
  stopping.conditions = setupTerminators(max.iter = 30L),
  monitor = myFancyConsoleMonitor
)
ctrl = setupEvolutionaryOperators(ctrl)

res = doTheEvolution(obj.fn, ctrl)
print(res)
