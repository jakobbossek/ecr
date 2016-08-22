context("opt path logging")

test_that("logging with ParamHelpers::OptPath works well", {
  n.population = 10L
  n.offspring = 4L
  max.iter = 3L

  fn = makeSphereFunction(2L)

  control = setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    representation = "float",
    logger = setupOptPathLoggingMonitor(),
    monitor = NULL,
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = max.iter))
  )

  res = doTheEvolution(fn, control = control)
  expect_is(res$opt.path, c("OptPath", "OptPathDF"))

  op = as.data.frame(res$opt.path)
  objective.name = res$task$objective.names
  decision.names = getParamIds(getParamSet(fn), with.nr = TRUE, repeated = TRUE)
  expect_equal(nrow(op), n.population * (max.iter + 1L)) # initial population + 3 generations

  # check contents for plausibility
  expect_true(all(objective.name %in% colnames(op)))
  expect_true(all(decision.names %in% colnames(op)))
  expect_true(max(op$dob) == max.iter)

  # check plots
  pl = autoplot(res, complete.trace = TRUE)
  expect_is(pl, "ggplot")

  pl = autoplot(res, complete.trace = TRUE, log.fitness = TRUE)
  expect_is(pl, "ggplot")

  # pl = autoplot(res, complete.trace = TRUE, show.process = TRUE)
  # expect_true(pl)
})

test_that("user defined extras are stored in OptPath", {
  fn = makeSphereFunction(10L)

  # define our extras fun which computes more or less useful stuff
  log.extras.fun = function(opt.state, ...) {
    population = opt.state$population
    list(
      "ex.num1" = sd(population$fitness), # stuff
      "ex.num2" = min(sapply(population$individuals, min)), # individuals
      "ex.disc" = sample(letters[1:10], 1L) # discrete stuff
    )
  }

  control = setupECRControl(
    n.population = 5L,
    n.offspring = 3L,
    survival.strategy = "plus",
    representation = "float",
    monitor = NULL,
    logger = setupOptPathLoggingMonitor(log.extras.fun = log.extras.fun),
    stopping.conditions = setupTerminators(max.iter = 2L)
  )

  res = doTheEvolution(fn, control)
  op = as.data.frame(res$opt.path)

  # check that corresponding column names exist
  expected.names = c("ex.num1", "ex.num2", "ex.disc")
  expect_true(isSubset(expected.names, colnames(op)))

  # check that they have the correct type
  expect_true(is.numeric(op[["ex.num1"]]))
  expect_true(is.numeric(op[["ex.num2"]]))
  expect_true(is.factor(op[["ex.disc"]]))
  expect_true(all(as.character(op[["ex.disc"]]) %in% letters[1:10]))
})

