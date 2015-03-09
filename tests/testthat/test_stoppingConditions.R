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

	control = ecr.control(
		population.size = 2L,
		offspring.size = 2L,
		n.params = 1L,
		survival.strategy = "plus",
		elite.size = 1L,
		representation = "float",
		monitor = makeNullMonitor(),
    stopping.conditions = setupStoppingConditions(max.iter = 5L)
	)

	# check for max time budget
	control$stopping.conditions = list(makeMaximumTimeStoppingCondition(max.time = 2))
	expect_true(grepl("Time limit", ecr(obj.fn, control)$message))

	# check for max iterations
	control$stopping.conditions = list(makeMaximumIterationsStoppingCondition(max.iter = 10L))
	expect_true(grepl("iterations", ecr(obj.fn, control)$message))
})
