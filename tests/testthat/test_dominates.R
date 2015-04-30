context("dominates and isDominated")

test_that("dominates and isDominated works as expected", {
  expect_true(dominates(c(1, 2), c(2, 3)))
  expect_true(isDominated(c(2, 3), c(1, 2)))
  expect_true(dominates(c(1, 2), c(1, 3)))
  expect_false(dominates(c(1, 2), c(1, 2)))
  expect_false(dominates(c(1, 2), c(2, 1)))
  expect_false(isDominated(c(1, 2), c(2, 1)))
  expect_false(dominates(c(843.3, 2313.3), c(42.2, 654.3324)))
})

test_that("[which.{non}]dominated works well on matrices", {
  m = matrix(
    c(# utopia point
      1, 1, 1,
      # dominated points
      2, 2, 3,
      2, 1, 2,
      2, 6, 1),
    byrow = TRUE, ncol = 3L
  )
  dom = dominated(m)
  # check that there is boolean value for each point
  expect_equal(length(dom), nrow(m))

  # check that we really have three dominated points
  expect_equal(sum(dom), 3L)

  # get indizes of dominated and nondominated points
  dom.idxs = which.dominated(m)
  nondom.idxs = which.nondominated(m)

  expect_true(setequal(dom.idxs, 2:4))
  expect_true(nondom.idxs == 1L)
  expect_true(setequal(c(dom.idxs, nondom.idxs), seq(nrow(m))))
})
