context("ecr main function")

setUpControlObject = function(n.population,
  n.offspring,
  survival.strategy = "plus",
  n.elite = 1L,
  n.mating.pool = round(n.population / 2),
  max.iter = 60L) {
  control = setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    survival.strategy = survival.strategy,
    n.elite = n.elite,
    representation = "float",
    monitor = NULL,
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = max.iter))
  )
  control = setupEvolutionaryOperators(
    control,
    survival.selector = setupGreedySelector()
  )
}

test_that("ecr works with simple soo function", {
  obj.fun = smoof::makeSphereFunction(dimensions = 2L)

  for (n.population in c(15, 30)) {
    for (n.offspring in c(15, 30)) {
      for (survival.strategy in c("plus", "comma")) {

        if (survival.strategy == "comma") {
          n.offspring = n.population
        }

        control = setUpControlObject(n.population, n.offspring, survival.strategy)
        res = doTheEvolution(obj.fun, control = control)
        expect_output(print(control), regexp = "CONTROL OBJECT")

        # check result
        expect_false(is.null(res))
        expect_true(res$best.value < 0.1,
          info = sprintf("Did not approximate optimal value with params mu: %i, lambda: %i, strategy: %s",
            n.population, n.offspring, survival.strategy))
        expect_true(all(res$best.param < 0.1),
          info = sprintf("Did not approximate optimal params with params mu: %i, lambda: %i, strategy: %s",
            n.population, n.offspring, survival.strategy))
        expect_output(print(res), regexp = "EA applied")
        expect_true(getGenerations(res) > 0L)
        expect_true(getEvaluations(res) > 0L)
      }
    }
  }
})

test_that("ecr works for maximization", {
  obj.fun = makeSingleObjectiveFunction(
    name = "maximize me",
    fn = function(x) -sum(x^2),
    par.set = makeNumericParamSet("x", len = 1L, lower = -10, upper = 10),
    minimize = FALSE # we want to maximize here
  )
  control = setupECRControl(
    n.population = 10L,
    n.offspring = 10L,
    survival.strategy = "plus",
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 50L)),
    monitor = NULL,
    representation = "float"
  )
  res = doTheEvolution(obj.fun, control = control)
  expect_true(abs(res$best.value - 0) < 0.05)
})

test_that("ecr works on binary representations", {
  n.params = 10L
  max.iter = 50L
  obj.fun = makeOneMinFunction(dimensions = n.params)

  for (n.population in c(10, 15)) {
    for (n.offspring in c(10, 15)) {
      for (mutator in c(setupBitFlipMutator())) {
        control = setupECRControl(
          n.population = n.population,
          n.offspring = n.offspring,
          survival.strategy = "plus",
          stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = max.iter)),
          monitor = NULL,
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

test_that("ecr works with additional arguments", {
  obj.fun = makeSingleObjectiveFunction(
    fn = function(x, shift = 100L) {
      sum(x^2) + shift
    },
    par.set = makeNumericParamSet("x", lower = -10, upper = 10, len = 1L)
  )
  control = setupECRControl(
    n.population = 10L,
    n.offspring = 5L,
    representation = "float",
    survival.strategy = "plus",
    monitor = NULL,
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 50L))
  )
  res = doTheEvolution(obj.fun, control, more.args = list(shift = 1000))
  expect_true(res$best.value < 1000.1)
  expect_true(res$best.param < 0.1)
})

test_that("ecr works on permutation genomes", {
  # defs
  n.params = 5L
  max.iter = 50L

  # objective
  obj.fun = makeSingleObjectiveFunction(
    fn = function(x) {
      CI = 0
      for (i in seq(length(x)-1)) {
        CI = CI + sum(x[1] > x[-1])
        x = x[-1]
      }
      return(CI)
    },
    par.set = makeParamSet(
      makeIntegerVectorParam(len = n.params, id = "x", lower = 1, upper = n.params)
    ),
    name = "Sorting"
  )

  control = setupECRControl(
    n.population = 5L,
    n.offspring = 5L,
    representation = "permutation",
    survival.strategy = "plus",
    monitor = NULL,
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 50L))
  )

  # check it for a selection of mutators for permutations
  for (mutatorGenerator in c(setupSwapMutator, setupInversionMutator, setupInsertionMutator)) {
    for (recombinatorGenerator in c(setupNullRecombinator, setupPMXRecombinator)) {
      control = setupEvolutionaryOperators(
        control,
        mutator = mutatorGenerator(),
        recombinator = recombinatorGenerator()
      )

      res = doTheEvolution(obj.fun, control = control)

      # check results
      expect_false(is.null(res))
      expect_equal(res$best.value, 0,
        info = sprintf("Did not find correct sorting with mutator '%s' and recombinator '%s'.",
          getOperatorName(control$mutator),
          getOperatorName(control$recombinator)
        )
      )
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
    monitor = NULL,
    stopping.conditions = setupTerminators(max.iter = 50L)
  )
  control = setupEvolutionaryOperators(control, mutator = setupGaussMutator(sdev = 0.05))

  res = doTheEvolution(fn, control = control)
  expect_true(res$best.value < 0.1)
  expect_true(all(res$best.param < 0.1))
})

test_that("ecr can handle initial populations", {
  fn = makeSphereFunction(2L)
  initial.population = list(c(1, 1), c(2, 2), c(3, 3))

  control = setupECRControl(
    n.population = 3L,
    n.offspring = 1L,
    representation = "float",
    monitor = NULL,
    stopping.conditions = setupTerminators(max.iter = 1L)
  )

  # stop if initial population is to large
  expect_error(doTheEvolution(fn, control, c(initial.population, c(2, 2.5))), "exceeds", ignore.case = TRUE)
})

test_that("ecr(...) shortcut function works as expected for floating point representation", {
  fn = function(x) {
    sum(x^2)
  }

  res = ecr(fn, n.dim = 2L, lower = c(-5, -5), upper = c(5, 5),
    representation = "float", n.population = 20L, n.offspring = 10L, max.iter = 30L,
    monitor = NULL)
  expect_true(abs(res$best.value) < 0.01)
  expect_true(all(res$best.param < 0.01))
})

test_that("ecr(...) shortcut function works as expected for binary point representation", {
  fn = function(x) {
    sum(x)
  }

  res = ecr(fn, n.dim = 15L, n.bits = 15L,
    representation = "binary", n.population = 20L, n.offspring = 10L, max.iter = 30L,
    monitor = NULL)
  expect_true(res$best.value == 0)
  expect_true(all(res$best.param == 0))
})
