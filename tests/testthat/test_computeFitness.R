context("compute fitness")

test_that("fitness values are computed correctly", {
  control = list(par.set = makeParamSet(makeIntegerParam(id = "x")))
  individuals = as.list(1:10)
  population = ecr:::makePopulation(individuals = individuals)
  fitness = ecr:::computeFitness(population, function(x) x^2, control)
  expect_true(is.matrix(fitness))
  expect_true(all.equal(as.numeric(fitness), (1:10)^2))
})
