context("recombination operators (recombinators)")

test_that("recombinators for permutations work as expected", {
  # defs
  n = 10L # permutation length
  n.reps = 5L # number of repetitions
  expected = seq(n) # sequence we expect offspring to be a permutation of

  # check validity of produced output for each permutation-based recombinator
  available.recombinators = c(setupPMXRecombinator, setupOXRecombinator)
  for (recombinatorGenerator in available.recombinators) {
    recombine = recombinatorGenerator()
    expect_true(isEcrOperator(recombine))
    expect_output(print(recombine), regexp = "Name")
    # generate sample permutations
    parent1 = sample(n)
    parent2 = sample(n)

    for (i in seq(n.reps)) {
      children = recombine(list(parent1, parent2))
      # check that the child is actually a permutation
      for (j in seq(length(children))) {
        expect_true(setequal(expected, children[[j]]), info = sprintf(
          "%i-th offspring ('%s') is not a permutation of sequence (%s) for operator '%s'",
        j,
        collapse(children[[j]]),
        collapse(expected),
        getOperatorName(recombine)))
      }
    }
  }
})
