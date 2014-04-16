context("ecr main function")

test_that("ecr works with simple soo function", {
  library(soobench)
  objective.fun = generate_sphere_function(2)
  control = ecr.control(
    population.size = 10L,
    offspring.size = 2L,
    max.iter = 100L,
    n.params = 2L,
    representation = "float",
    show.info = FALSE)
  res = ecr(objective.fun, control)
  expect_false(is.null(res))
})
