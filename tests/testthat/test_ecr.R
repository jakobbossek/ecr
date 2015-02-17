context("ecr main function")

test_that("ecr works with simple soo function", {
  obj.fun = makeSingleObjectiveFunction(
    name = "2D Sphere",
    fn = function(x) sum(x^2),
    par.set = makeParamSet(
      makeNumericParam("x1", lower = -2, upper = 2),
      makeNumericParam("x2", lower = -2, upper = 2)
    )
  )

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
    max.iter = max.iter,
    n.params = 2L,
    representation = "float",
    monitor = makeNullMonitor(),
    stoppingConditions = list(makeMaximumIterationsStoppingCondition(max.iter = max.iter))
  )
  }

  for (population.size in c(20, 40, 60)) {
    for (offspring.size in c(20, 40, 60)) {
      for (survival.strategy in c("plus", "comma")) {

        if (survival.strategy == "comma") {
          offspring.size = population.size
        }

        control = setUpControlObject(population.size, offspring.size, survival.strategy)
        res = ecr(obj.fun, control = control)

        # check result
        expect_false(is.null(res))
        expect_true(res$best.value < 0.01,
          info = sprintf("Did not approximate optimal value with params mu: %i, lambda: %i, strategy: %s", population.size, offspring.size, survival.strategy))
        expect_true(all(res$best.param < 0.1),
          info = sprintf("Did not approximate optimal params with params mu: %i, lambda: %i, strategy: %s", population.size, offspring.size, survival.strategy))
      }
    }
  }
})
