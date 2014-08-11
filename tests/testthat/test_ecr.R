context("ecr main function")

test_that("ecr works with simple soo function", {
  library(soobench)
  obj.fun = makeSingleObjectiveFunction(
    name = "2D Sphere",
    fn = function(x) sum(x^2),
    par.set = makeParamSet(
      makeNumericParam("x1", lower = -2, upper = 2),
      makeNumericParam("x2", lower = -2, upper = 2)
    )
  )

  control = ecr.control(
    population.size = 10L,
    offspring.size = 2L,
    max.iter = 100L,
    n.params = 2L,
    representation = "float",
    show.info = FALSE
  )

  res = ecr(obj.fun, control = control)
  expect_false(is.null(res))
})
