context("esoo main function")

test_that("esoo works with simple soo function", {
  library(soobench)
  objective.fun = generate_sphere_function(2)
  control = esoo.control(
    population.size = 10L,
    offspring.size = 2L,
    max.iter = 100L,
    n.params = 2L,
    representation = "float")
  res = esoo(objective.fun, control)
  expect_false(is.null(res))
})
