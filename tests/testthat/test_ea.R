context("Single-Objective Algorithms")

test_that("simpleEA work well", {
  fn = makeSphereFunction(2L)
  res = simpleEA(fn, n.population = 30L, max.iter = 30L, monitor = NULL)
  expect_is(res, "ecr_single_objective_result")
  expect_true(abs(res$best.value - getGlobalOptimum(fn)$value) < 0.1)
})

test_that("(1+1) GA works well", {
  gene.length = 10L
  fn = makeSingleObjectiveFunction(
    name = "One-Max",
    fn = function(x) length(x) - sum(x),
    par.set = makeNumericParamSet("bin", lower = 0, upper = 1, len = gene.length)
  )
  res = onePlusOneGA(fn, max.iter = 100L, monitor = NULL)
  expect_is(res, "ecr_single_objective_result")
  expect_true(res$best.value == 0)
  expect_equal(sum(res$best.param), gene.length)
})
