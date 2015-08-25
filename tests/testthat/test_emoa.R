context("Evolutionary Multi-Objective Algorithms")

test_that("preimplemented EMOAs work well", {
  fn = smoof::makeZDT1Function(dimensions = 2L)
  res = nsga2(
    makeOptimizationTask(fn),
    n.population = 10L,
    n.offspring = 3L,
    max.evals = 100L
  )
  pf = res$pareto.front
  print(pf)
  expect_equal(nrow(pf), length(which.nondominated(t(pf))))
  expect_true(all(is.numeric(pf)))
  expect_equal(ncol(pf), getNumberOfObjectives(fn))
})
