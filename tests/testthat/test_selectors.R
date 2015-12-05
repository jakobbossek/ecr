context("selectors get a population and return part of it")

test_that("parent selectors work as expected", {
  task = list(par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 5L)))
  population = lapply(1:10, function(i) runif(5L))
  population = makePopulation(population, fitness = NULL)
  fitness = computeFitness(population, sum, task, NULL)
  population$fitness = fitness

  n.mating.pool = 5L

  avialable.selectors = c(makeGreedySelector,
    makeRouletteWheelSelector, makeTournamentSelector
  )

  for (selectorGenerator in avialable.selectors) {
    select = selectorGenerator()
    mating.pool = select(population, NULL, NULL, n.mating.pool, NULL)
    expect_is(mating.pool, "ecrPopulation")
    expect_true(is.list(mating.pool$individuals))
    expect_true(is.numeric(mating.pool$fitness))
    expect_equal(length(mating.pool$fitness), n.mating.pool)
    expect_true(isSubset(mating.pool$fitness, population$fitness))
    expect_equal(length(mating.pool$individuals), n.mating.pool)
  }

  # simple mating pool selector simply returns the population
  select = makeSimpleSelector()
  mating.pool = select(population, list(), n.mating.pool)
  expect_true(is.list(mating.pool$individuals))
  expect_equal(length(mating.pool$individuals), length(population$individuals))
})
