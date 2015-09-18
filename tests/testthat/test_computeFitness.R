context("compute fitness")

test_that("fitness values are computed correctly", {
  control = list(par.set = makeParamSet(makeIntegerParam(id = "x")))
  individuals = matrix(1:10, nrow = 10, ncol = 1)
  population = ecr:::makePopulation(individuals = individuals)
  fitness = ecr:::computeFitness(population, function(x) x^2, control)
  expect_true(is.matrix(fitness))
  expect_true(all.equal(as.numeric(fitness), (1:10)^2))
})
