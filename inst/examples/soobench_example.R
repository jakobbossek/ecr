# Optimization of the famous Ackley function in one dimension
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
library(soobench)
library(ggplot2)
library(BBmisc)

load_all(".", reset=TRUE)

# Monitoring function. For details on the expected formal parameters
# see the help pages for makeMonitor and makeConsoleMonitor.
myMonitorStep = function(objective.fun, population, trace, iter, control) {
  n.params = control$n.params
  n.targets = control$n.targets
  if (!(n.params == 1 && n.targets == 1)) {
    warningf("Monitor cannot handle multidimensional funs.")
  }
  x = seq(-35, 35, by = 0.05)
  df = data.frame(x = x, y = sapply(x, objective.fun))
  pl = ggplot(data = df, aes(x = x, y = y)) + geom_line()

  population.points = data.frame(x = as.numeric(population$population[, 1]), y = as.numeric(population$fitness))
  pl = pl + geom_point(data = population.points, colour = "tomato", size = 2.2)
  pl = pl + geom_hline(yintercept = min(population$fitness), linetype = "dotted", colour = "tomato")
  print(pl)
  pause()
}

myMonitor = makeMonitor(step = myMonitorStep)

# generate our target function
objective.fun = generate_ackley_function(1)

# initialize control object
control = ecr.control(
  population.size = 20L,
  offspring.size = 5L,
  max.iter = 15L,
  survival.strategy = "plus",
  representation = "float",
  n.params = 1L,
  n.targets = 1L,
  mutator.control = list(mutator.gauss.sd = 0.005),
  monitor = myMonitor)

# do the evolutionary magic
set.seed(123)

res = ecr(objective.fun, control = control)

