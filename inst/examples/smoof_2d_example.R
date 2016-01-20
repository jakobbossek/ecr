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
  population = envir$opt.state$population
  task = envir$opt.state$task

  x = seq(-5, 5, by = 0.05)
  df = expand.grid(x, x)
  names(df) = paste0("x", 1:2)
  df.points = as.data.frame(do.call(rbind, population$individuals))
  names(df.points) = names(df)
  df$y = apply(df, 1L, task$fitness.fun)

  pl = ggplot(data = df, aes(x = x1, y = x2, z = y)) + geom_contour(colour = "gray")
  pl = pl + geom_point(data = df.points, aes(z = NULL), colour = "tomato")
  print(pl)
}

myMonitor = makeMonitor(step = myMonitorStep)

# generate objective function
obj.fun = makeRastriginFunction(dimensions = 2L)

# initialize control object
control = setupECRControl(
  n.population = 100L,
  n.offspring = 10L,
  survival.strategy = "plus",
  representation = "float",
  monitor = myMonitor,
  stopping.conditions = setupStoppingConditions(max.iter = 100L)
)
# use default operators
control = setupEvolutionaryOperators(control, mutator = makeGaussMutator(p = 1, sdev = 0.14))

# do the evolutionary magic
set.seed(123)
res = doTheEvolution(obj.fun, control = control)
