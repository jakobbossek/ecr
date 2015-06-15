context("Hypervolume contribution")

test_that("calculation of dominated hypervolume works as expected", {
  #FIXME: extend this test case!
  # here all the points in the approximation are located on a line
  # |
  # |               o (6,6)
  # |  o  (1,5)
  # |    o (2,4)
  # |      o (3,3)
  # |        o (4,2)
  # |          o (5,1)
  # __________________
  # The correct/expected dominated hypervolume value is 15 in this test case
  hv.exp = 15
  points = matrix(
    c(1, 5,
      2, 4,
      3, 3,
      4, 2,
      5, 1),
    nrow = 2L)
  ref.point = c(6, 6)

  hv = computeDominatedHypervolume(points, ref.point)
  expect_true(is.numeric(hv))
  expect_equal(hv, hv.exp)

  # now check the hypervolume contributions
  hv.contribs = computeHypervolumeContribution(points, ref.point)
  expect_true(all(hv.contribs == 1))
  # the computed raference point is should be equal to (6,6) too
  hv.contribs = computeHypervolumeContribution(points)
  expect_true(all(hv.contribs == 1))
})
