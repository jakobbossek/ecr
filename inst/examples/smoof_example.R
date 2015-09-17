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
  n.targets = envir$control$n.targets
  population = envir$population

  x = seq(-5, 5, by = 0.05)
  df = data.frame(x = x, y = sapply(x, envir$task$fitness.fun))
  pl = ggplot(data = df, aes(x = x, y = y)) + geom_line()

  population.points = data.frame(x = unlist(population$individuals), y = as.numeric(population$fitness))
  pl = pl + geom_point(data = population.points, colour = "tomato", size = 2.2)
  pl = pl + geom_hline(yintercept = min(population$fitness), linetype = "dotted", colour = "tomato")
  print(pl)
  Sys.sleep(0.3)
}

myMonitor = makeMonitor(step = myMonitorStep)

# generate objective function
obj.fun = makeRastriginFunction(dimensions = 1L)

# initialize control object
control = setupECRControl(
  n.population = 20L,
  n.offspring = 5L,
  survival.strategy = "plus",
  representation = "float",
  monitor = myMonitor,
  stopping.conditions = setupStoppingConditions(max.iter = 25L)
)
# use default operators
control = setupEvolutionaryOperators(control, mutator = makeGaussMutator(prob = 0.4, sdev = 0.000014))

# do the evolutionary magic
set.seed(123)
res = doTheEvolution(obj.fun, control = control)
