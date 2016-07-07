library(ecr)
library(smoof)
library(parallelMap)
library(microbenchmark)

# In the following we perform a simple benchmark on an artificially slowed down
# objective function to show the effect of parallel function evaluation.
fn = makeSingleObjectiveFunction(
  fn = function(x) {
    Sys.sleep(runif(1, min = 0.03, max = 0.1)) # delay execution a bit
    return(sum(x^2))
  },
  par.set = makeNumericParamSet("x", len = 2L, lower = -5, upper = 5)
)

control = setupECRControl(
  n.population = 10L, n.offspring = 10L,
  survival.strategy = "plus",
  representation = "float",
  stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 30L))
)

# wrapper for sequential evaluation
EAseq = function() {
  doTheEvolution(fn, control)
}

# wrapper for parallel evaluation with 3 CPU cores
# Note: This will fail, if your system has less than 3 cores.
EApar = function() {
  parallelStartMulticore(cpus = 3L, level = "ecr.evaluateFitness") # use 3 cpus
  st.par = system.time({doTheEvolution(fn, control)})
  parallelStop()
}

set.seed(1)
# Use the microbenchmark package to repeat both sequential and parallel EA
# each 10 times.
bench = microbenchmark(
  EAseq(),
  EApar(),
  times = 10L
)

# We see a speedup of about factor 3
print(bench, unit = "relative")
pl = autoplot(bench, log = FALSE) + ylim(c(0, 50)) + ylab("Time [seconds]")
print(pl)
# Unit: relative
#     expr   min    lq  mean median    uq  max neval cld
#  EAseq() 2.429 2.349 2.379   2.36 2.346 2.42    10   b
#  EApar() 1.000 1.000 1.000   1.00 1.000 1.00    10  a
