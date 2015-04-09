context("permutation generator")

test_that("permutation generator works properly", {
  
  permGen <- ecr:::makePermutationGenerator()

  set.seed(5678)
  population <- permGen(3, 7)
  
  expect_true(all.equal(attributes(population)$names, c("individuals", "fitness")))
  expect_true(all.equal(attributes(population)$classes, c("ecrPopulation", "setOfIndividuals")))

  popComp <- matrix(0, nrow = 3, ncol = 7)
  set.seed(5678)
  for (i in 1:3) {
    popComp[i, ] <- sample(1:7)
  }
  expect_true(all.equal(population$individuals, popComp))
})
