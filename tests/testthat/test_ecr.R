context("ecr main function")

test_that("ecr works with simple soo function", {
  library(soobench)
  obj.fun = sphere_function(2)
  par.set = extractParamSetFromSooFunction(obj.fun)
  control = ecr.control(
    population.size = 10L,
    offspring.size = 2L,
    max.iter = 100L,
    n.params = 2L,
    representation = "float",
    show.info = FALSE)
  res = ecr(obj.fun, par.set = par.set, control = control)
  expect_false(is.null(res))
})
