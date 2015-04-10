context("autoplot")

test_that("autoplot standard plot", {
  n.params = 10L
  # use maximise of 1's as example
  obj.fun = makeSingleObjectiveFunction(
    fn = function(x) {length(x) - sum(x)},
    par.set = makeParamSet(
      makeIntegerVectorParam(len = n.params, id = "x", lower = 0, upper = 1)
    ),
    name = "ONE-MAX"
  )
  control = ecr.control(
    population.size = 5L,
    offspring.size = 5L,
    representation = "binary",
    survival.strategy = "plus",
    n.params = n.params,
    generator = makeBinaryGenerator(),
    mutator = makeBitFlipMutator(),
    recombinator = makeCrossoverRecombinator(),
    stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 15L))
  )
  res = ecr(obj.fun, control = control)
  expect_true(autoplot(res, complete.trace = TRUE))
})

test_that("autoplot for log axis and show process", {
  for (n.params in c(1L, 2L)) {
    # now we wrap the objective function with the smoof package
    obj.fun = makeSingleObjectiveFunction(
      fn = function(x) {sum(x^2)},
      par.set = makeParamSet(
        makeNumericVectorParam(len = n.params, id = "x", lower = -4, upper = 4)
      ),
      name = "Parabola"
    )
    control = ecr.control(
      population.size = 5L,
      offspring.size = 5L,
      representation = "float",
      survival.strategy = "plus",
      n.params = n.params,
      # FIXME: Throws an error if not complete population is saved
      save.population.at = 0:15,
      mutator = makeGaussMutator(mutator.gauss.sd = 0.005),
      recombinator = makeCrossoverRecombinator(),
      stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 15L))
    )
    res = ecr(obj.fun, control = control)
    expect_message(autoplot(res, log.fitness = TRUE, complete.trace = TRUE)
                   , regexp = "Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale.")
    expect_true(autoplot(res, show.process = TRUE, complete.trace = TRUE))
  }
})
