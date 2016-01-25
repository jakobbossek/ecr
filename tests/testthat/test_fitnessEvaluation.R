context("evaluate fitness")

test_that("fitness evaluations work the expected way", {
  task = list(par.set = makeParamSet(makeIntegerParam(id = "x")))
  individuals = as.list(1:10)
  population = ecr:::makePopulation(individuals = individuals)
  fitness = ecr:::evaluateFitness(population, function(x) x^2, task, list(vectorized.evaluation = FALSE))
  expect_true(is.matrix(fitness))
  expect_true(all.equal(as.numeric(fitness), (1:10)^2))
})

test_that("vectorized fitness evaluations works for smoof functions", {
  control = setupECRControl(
    n.population = 10L,
    n.offspring = 10L,
    survival.strategy = "plus",
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 50L)),
    monitor = NULL,
    representation = "float",
    vectorized.evaluation = TRUE
  )

  obj.fun = makeSphereFunction(2L)
  expect_error({doTheEvolution(obj.fun, control = control)}, regexp = "not vectorized")

  obj.fun = makeBBOBFunction(fid = 1L, iid = 1L, dimension = 2L)
  res = doTheEvolution(obj.fun, control = control)
  expect_true(abs(res$best.value - getGlobalOptimum(obj.fun)$value) < 0.05)
})

test_that("vectorized fitness evaluations works for custom representations", {
  control = setupECRControl(
    n.population = 10L,
    n.offspring = 10L,
    survival.strategy = "plus",
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 50L)),
    monitor = NULL,
    representation = "custom",
    vectorized.evaluation = TRUE
  )

  # since I do not want to generate all that custom stuff here we
  # create a custom representation problem which indeed is a float representation
  # problem (optimize 1D sphere the hard way)

  # Define the vectorized shifted sphere function (shifted to be able to check if
  # the vectorized code is really executed).
  obj.fun = function(xs, ...) {
    # evaluate entire "vector"
    res = unlist(lapply(xs, function(x) {
      sum(x^2) + 1L # location shift
    }))
    return(res)
  }

  # setup float operators and modify their supported representation
  myGenerator = setupUniformGenerator()
  attr(myGenerator, "supported") = "custom"
  myMutator = setupGaussMutator()
  attr(myMutator, "supported") = "custom"

  # default stuff
  control = setupEvolutionaryOperators(
    control,
    parent.selector = setupTournamentSelector(),
    generator = myGenerator,
    mutator = myMutator,
    recombinator = setupNullRecombinator(),
    survival.selector = setupGreedySelector()
  )

  # since we are custom we need to generate a task by hand
  task = makeOptimizationTask(obj.fun, n.objectives = 1L)
  task$par.set = getParamSet(makeSphereFunction(1L))

  res = doTheEvolution(task, control = control)
  expect_true(abs(res$best.value - 1) < 0.05)
})
