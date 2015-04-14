context("ecr main function")

setUpControlObject = function(n.population,
  n.offspring,
  survival.strategy = "plus",
  n.elite = 1L,
  n.mating.pool = round(n.population / 2),
  max.iter = 100L) {
  control = setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    survival.strategy = survival.strategy,
    n.elite = n.elite,
    n.params = 2L,
    representation = "float",
    monitor = makeNullMonitor(),
    stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = max.iter))
  )
  control = setupEvolutionaryOperators(control)
}

test_that("ecr works with simple soo function", {
  obj.fun = smoof::makeSphereFunction(dimensions = 2L)

  for (n.population in c(10, 20)) {
    for (n.offspring in c(10, 20)) {
      for (survival.strategy in c("plus", "comma")) {

        if (survival.strategy == "comma") {
          n.offspring = n.population
        }

        control = setUpControlObject(n.population, n.offspring, survival.strategy)
        res = doTheEvolution(obj.fun, control = control)

        # check result
        expect_false(is.null(res))
        expect_true(res$best.value < 0.1,
          info = sprintf("Did not approximate optimal value with params mu: %i, lambda: %i, strategy: %s",
            n.population, n.offspring, survival.strategy))
        expect_true(all(res$best.param < 0.1),
          info = sprintf("Did not approximate optimal params with params mu: %i, lambda: %i, strategy: %s",
            n.population, n.offspring, survival.strategy))
      }
    }
  }
})

test_that("ecr works on binary representations", {
  n.params = 10L
  max.iter = 150L
  obj.fun = makeOneMinFunction(dimensions = n.params)

  for (n.population in c(10, 15)) {
    for (n.offspring in c(10, 15)) {
      for (mutator in c(makeBitFlipMutator())) {
        control = setupECRControl(
          n.population = n.population,
          n.offspring = n.offspring,
          survival.strategy = "plus",
          stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = max.iter)),
          monitor = makeNullMonitor(),
          n.params = n.params,
          representation = "binary",
        )
        control = setupEvolutionaryOperators(
          control,
          mutator = mutator
        )

        res = doTheEvolution(obj.fun, control = control)

        # check results
        expect_false(is.null(res))
        expect_equal(res$best.value, 0,
          info = sprintf("Did not find OneMin minimum with params mu: %i, lambda: %i, strategy: %s, mutator: %s",
            n.population, n.offspring, "plus", getOperatorName(mutator)))
        expect_true(all(res$best.param == 0),
          info = sprintf("Did not find OneMin minimum with params mu: %i, lambda: %i, strategy: %s, mutator: %s",
            n.population, n.offspring, "plus", getOperatorName(mutator)))
      }
    }
  }
})

test_that("ecr finds optimum if is is located on the edge of the search space", {
  fn = makeSingleObjectiveFunction(
    name = "linear",
    par.set = makeNumericParamSet("x", len = 2L, lower = 0, upper = 1),
    # optimum is in (0, 0)
    fn = function(x) sum(x)
  )

  # initialize control object
  control = setupECRControl(
    n.population = 30L,
    n.offspring = 10L,
    survival.strategy = "plus",
    representation = "float",
    monitor = makeNullMonitor(),
    n.params = 2L,
    stopping.conditions = setupStoppingConditions(max.iter = 100L)
  )
  control = setupEvolutionaryOperators(control, mutator.control = list(mutator.gauss.sd = 0.05))

  res = doTheEvolution(fn, control = control)
  expect_true(res$best.value < 0.1)
  expect_true(all(res$best.param < 0.1))
})
