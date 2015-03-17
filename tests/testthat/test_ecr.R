context("ecr main function")

setUpControlObject = function(population.size,
  offspring.size,
  survival.strategy = "plus",
  elite.size = 1L,
  mating.pool.size = round(population.size / 2),
  max.iter = 100L) {
  ecr.control(
    population.size = population.size,
    offspring.size = offspring.size,
    survival.strategy = survival.strategy,
    elite.size = elite.size,
    n.params = 2L,
    representation = "float",
    monitor = makeNullMonitor(),
    stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = max.iter))
    )
}

test_that("ecr works with simple soo function", {
  obj.fun = smoof::makeSphereFunction(dimensions = 2L)

  for (population.size in c(10, 20)) {
    for (offspring.size in c(10, 20)) {
      for (survival.strategy in c("plus", "comma")) {

        if (survival.strategy == "comma") {
          offspring.size = population.size
        }

        control = setUpControlObject(population.size, offspring.size, survival.strategy)
        res = ecr(obj.fun, control = control)

        # check result
        expect_false(is.null(res))
        expect_true(res$best.value < 0.1,
          info = sprintf("Did not approximate optimal value with params mu: %i, lambda: %i, strategy: %s", population.size, offspring.size, survival.strategy))
        expect_true(all(res$best.param < 0.1),
          info = sprintf("Did not approximate optimal params with params mu: %i, lambda: %i, strategy: %s", population.size, offspring.size, survival.strategy))
      }
    }
  }
})

test_that("ecr works on binary representations", {
  n.params = 10L
  max.iter = 150L
  obj.fun = makeOneMinFunction(dimensions = n.params)

  for (population.size in c(10, 15)) {
    for (offspring.size in c(10, 15)) {
      for (mutator in c(makeBitFlipMutator())) {
        control = ecr.control(
          population.size = population.size,
          offspring.size = offspring.size,
          survival.strategy = "plus",
          stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = max.iter)),
          monitor = makeNullMonitor(),
          n.params = n.params,
          generator = makeBinaryGenerator(),
          recombinator = makeCrossoverRecombinator(),
          representation = "binary",
          mutator = mutator
        )

        res = ecr(obj.fun, control = control)

        # check results
        expect_false(is.null(res))
        expect_equal(res$best.value, 0,
          info = sprintf("Did not find OneMin minimum with params mu: %i, lambda: %i, strategy: %s, mutator: %s",
            population.size, offspring.size, "plus", getOperatorName(mutator)))
        expect_true(all(res$best.param == 0),
          info = sprintf("Did not find OneMin minimum with params mu: %i, lambda: %i, strategy: %s, mutator: %s",
            population.size, offspring.size, "plus", getOperatorName(mutator)))
      }
    }
  }
})
