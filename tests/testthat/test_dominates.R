context("dominates and isDominated")

test_that("dominates and isDominated works as expected", {
  expect_true(dominates(c(1, 2), c(2, 3)))
  expect_true(isDominated(c(2, 3), c(1, 2)))
  expect_true(dominates(c(1, 2), c(1, 3)))
  expect_false(dominates(c(1, 2), c(1, 2)))
  expect_false(dominates(c(843.3, 2313.3), c(42.2, 654.3324)))
})
