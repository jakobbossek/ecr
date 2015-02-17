context("termination codes")

test_that("termination codes do work", {
	obj.fn = makeSingleObjectiveFunction(
		name = "1D Sphere",
		fn = function(x) sum(x^2),
		par.set = makeParamSet(
			makeNumericParam("x", lower = -2, upper = 2)
		),
		global.opt.params = list(x = 0),
		global.opt.value = 0
	)

	makeControlForTestCase = function(max.iter = 100000L, max.time = 3600L, termination.eps = 0) {
		ecr.control(
			population.size = 20L,
			offspring.size = 20L,
			n.params = 1L,
			survival.strategy = "plus",
			elite.size = 1L,
			representation = "float",
			show.info = FALSE,
			max.iter = max.iter,
			max.time = max.time,
			termination.eps = termination.eps
		)
	}

	# check for max iterations
	control = makeControlForTestCase(max.iter = 5L)
	expect_equal(ecr(obj.fn, control)$convergence, 0L)

	# check for reached tolerence level
	set.seed(1)
	control = makeControlForTestCase(termination.eps = 2)
	expect_equal(ecr(obj.fn, control)$convergence, 1L)

	# check for max time budget
	control = makeControlForTestCase(max.time = 2L)
	expect_equal(ecr(obj.fn, control)$convergence, 2L)
})