context("recombination operators (recombinators)")

test_that("recombinators for permutations work as expected", {
  # defs
  n = 10L # permutation length
  n.reps = 5L # number of repetitions
  expected = seq(n) # sequence we expect offspring to be a permutation of

  # check validity of produced output for each permutation-based recombinator
  available.recombinators = c(makePMXRecombinator, makePMXRecombinator)
  for (recombinatorGenerator in available.recombinators) {
    recombine = recombinatorGenerator()
    # generate sample permutations
    parent1 = sample(n)
    parent2 = sample(n)

    for (i in seq(n.reps)) {
      child = recombine(list(parent1, parent2))
      # check that the child is actually a permutation
      expect_true(setequal(expected, child), info = sprintf(
        "Offspring ('%s') is not a permutation of sequence (%s) for operator '%s'",
        collapse(child),
        collapse(expected),
        getOperatorName(recombine))
      )
    }
  }
})
