context("selectors get a population and return part of it")

test_that("parent selectors work as expected", {
  population = lapply(1:10, function(i) runif(5L))
  population = makePopulation(population, fitness = NULL)
  fitness = computeFitness(population, sum)
  population$fitness = fitness

  n.mating.pool = 5L

  for (selectorGenerator in c(makeRouletteWheelSelector, makeTournamentSelector)) {
    select = selectorGenerator()
    mating.pool = select(population, list(), n.mating.pool)
    expect_is(mating.pool, "ecrPopulation")
    expect_true(is.list(mating.pool$individuals))
    expect_true(is.numeric(mating.pool$fitness))
    expect_equal(length(mating.pool$fitness), n.mating.pool)
    expect_true(isSubset(mating.pool$fitness, population$fitness))
    expect_equal(length(mating.pool$individuals), n.mating.pool)
  }
})
