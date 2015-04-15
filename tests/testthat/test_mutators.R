context("mutation operators (mutators)")

test_that("mutation operators working on permutation genes create valid offspring", {
  # defs
  s = 1:10L
  n.reps = 5L

  # check validity of produced output for each permutation-based mutator
  for (mutatorGenerator in c(makeSwapMutator, makeInversionMutator, makeInsertionMutator)) {
    mutate = mutatorGenerator() # no mutation control parameters to check here
    test.seq = sample(seq(100L), 10L, replace = FALSE)
    for (i in seq(n.reps)) {
      perm.seq = mutate(test.seq)
      expect_true(setequal(test.seq, perm.seq), info = sprintf("Mutator '%s' did not produce a valid
        permutation! Input: (%s), Output: (%s)", getOperatorName(mutate),
        collapse(test.seq), collapse(perm.seq)))
    }
  }
})
