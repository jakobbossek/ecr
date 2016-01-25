context("termination codes")

test_that("stopping conditions work", {
	obj.fn = makeSingleObjectiveFunction(
		name = "1D Sphere",
		fn = function(x) sum(x^2),
		par.set = makeParamSet(
			makeNumericParam("x", lower = -2, upper = 2)
		),
		global.opt.params = list(x = 0),
		global.opt.value = 0
	)

	control = setupECRControl(
		n.population = 50L,
		n.offspring = 2L,
		survival.strategy = "plus",
		n.elite = 1L,
		representation = "float",
		monitor = NULL,
    stopping.conditions = setupTerminators(max.iter = 10L)
	)
  control = setupEvolutionaryOperators(control)

	# check for max time budget
	control$stopping.conditions = list(setupMaximumTimeTerminator(max.time = 2))
	expect_true(grepl("Time limit", doTheEvolution(obj.fn, control)$message))

	# check for max iterations
	control$stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = 10L))
	expect_true(grepl("iterations", doTheEvolution(obj.fn, control)$message))

  # check for max evaluations
  control$stopping.conditions = list(setupMaximumEvaluationsTerminator(max.evals = 10L))
  expect_true(grepl("evaluations", doTheEvolution(obj.fn, control)$message))

  # check closeness to optimum
  control$stopping.conditions = list(setupCloseToOptimumTerminator(eps = 0.05, opt = getGlobalOptimum(obj.fn)$value))
  expect_true(grepl("optimum", doTheEvolution(obj.fn, control)$message))
})
