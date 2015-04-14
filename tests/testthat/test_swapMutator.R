context("swap mutator")

test_that("permutation example with swap mutator", {
  n.params = 7L
  obj.fun = makeSingleObjectiveFunction(
    fn = function(x) {
      CI = 0
      for (i in seq(length(x)-1)) {
        CI = CI + sum(x[1] > x[-1])
        x = x[-1]
      }
      return(CI)
    },
    par.set = makeParamSet(
      makeIntegerVectorParam(len = n.params, id = "x", lower = 1, upper = n.params)
    ),
    name = "Sorting"
  )
  control = setupECRControl(
    n.population = 5L,
    n.offspring = 5L,
    representation = "permutation",
    survival.strategy = "plus",
    monitor = makeNullMonitor(),
    stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 50L))
  )
  control = setupEvolutionaryOperators(control)
  res = doTheEvolution(obj.fun, control = control)
  # check results
  expect_false(is.null(res))
  expect_equal(res$best.value, 0,
               info = sprintf("Did not find correct sorting with swap mutator in permutation."))
})
