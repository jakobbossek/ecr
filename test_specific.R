library(methods)
library(devtools)
library(testthat)
library(soobench)

load_all(".")

fn = generate_ackley_function(1)

control = ecr.control(
  population.size = 20L,
  offspring.size = 50L,
  representation = "float",
  survival.strategy = "plus",
  elite.size = 1L,
  n.params = 1L,
  n.targets = 1L,
  show.info = TRUE,
  termination.eps = 0.0001,
  max.iter = 200L,
  show.info.stepsize = 5L)

res = ecr(fn, control, global.optimum = 0L)
