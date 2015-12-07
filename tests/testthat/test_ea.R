context("Single-Objective Algorithms")

test_that("preimplemented EAs work well", {
  fn = makeSphereFunction(2L)
  res = simpleEA(fn, n.population = 15L, max.iter = 30L, monitor = makeNullMonitor())
  expect_is(res, "ecr_single_objective_result")
  expect_true(abs(res$best.value - getGlobalOptimum(fn)$value) < 0.1)
})
