context("selectors get a population and return part of it")

test_that("parent selectors work as expected", {
  task = list(par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 5L)))
  population = lapply(1:10, function(i) runif(5L))
  population = makePopulation(population, fitness = NULL)
  fitness = evaluateFitness(population, sum, task, list(vectorized.evaluation = FALSE))
  population$fitness = fitness

  n.mating.pool = 5L

  avialable.selectors = c(setupGreedySelector,
    setupRouletteWheelSelector, setupTournamentSelector
  )

  for (selectorGenerator in avialable.selectors) {
    select = selectorGenerator()
    mating.pool.idx = select(population$fitness, n.mating.pool, NULL, NULL, NULL)
    expect_true(is.integer(mating.pool.idx))
    expect_equal(length(mating.pool.idx), n.mating.pool)
    expect_true(isSubset(mating.pool.idx, 1:length(fitness)))
  }

  # simple mating pool selector simply returns the population
  select = setupSimpleSelector()
  mating.pool.idx = select(fitness, n.mating.pool, NULL, NULL, NULL)
  expect_true(is.integer(mating.pool.idx))
  expect_equal(length(mating.pool.idx), length(fitness))
})
