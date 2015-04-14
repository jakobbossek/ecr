# Optimization of the famous Rastrigin function in one dimension
# using a simple Evolutionary Strategy (ES) and a custom
# monitoring function, which pauses the optimization process
# after each generation and plots the target fun as well as
# the entire population.

# We choose a (20 + 5) strategy with 1-elitism, a natural real-valued
# representation of the variables and 0.005 as the standard deviation
# of the normal mutation operator.
library(methods)
library(testthat)
library(devtools)
library(smoof)
library(ggplot2)
library(BBmisc)

load_all(".", reset = TRUE)

# Monitoring function. For details on the expected formal parameters
# see the help pages for makeMonitor and makeConsoleMonitor.
myMonitorStep = function(envir = parent.frame()) {
  n.params = envir$control$n.params
  n.targets = envir$control$n.targets
  population = envir$population
  if (!(n.params == 1 && is.null(n.targets))) {
    warningf("Monitor cannot handle multidimensional funs.")
  }
  x = seq(-5, 5, by = 0.05)
  df = data.frame(x = x, y = sapply(x, envir$objective.fun))
  pl = ggplot(data = df, aes(x = x, y = y)) + geom_line()

  population.points = data.frame(x = unlist(population$individuals), y = as.numeric(population$fitness))
  pl = pl + geom_point(data = population.points, colour = "tomato", size = 2.2)
  pl = pl + geom_hline(yintercept = min(population$fitness), linetype = "dotted", colour = "tomato")
  print(pl)
  Sys.sleep(0.3)
}

myMonitor = makeMonitor(step = myMonitorStep)

# generate objective function
obj.fun = makeRastriginFunction(dimensions = 3L)

# initialize control object
control = setupECRControl(
  n.population = 20L,
  n.offspring = 5L,
  survival.strategy = "plus",
  representation = "float",
  n.params = 3L,
  monitor = myMonitor,
  stopping.conditions = setupStoppingConditions(max.iter = 25L)
)
control = setupEvolutionaryOperators(control, mutator.control = list(mutator.gauss.sd = 0.005))

# do the evolutionary magic
set.seed(123)

res = doTheEvolution(obj.fun, control = control)
