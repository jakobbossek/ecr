context("compute fitness")

test_that("fitness values are computed correctly", {
  individuals = matrix(1:10, nrow = 10, ncol = 1)
  population = ecr:::makePopulation(individuals = individuals)
  population$fitness = ecr:::computeFitness(population, function(x) x^2)
  expect_true(all.equal(population$fitness, (1:10)^2))
})
