context("mutation operators (mutators)")

test_that("mutation operators working on permutation genes create valid offspring", {
  # defs
  s = 1:10L
  n.reps = 5L

  # gather all mutators for permutation representation
  available.mutators = c(
    makeSwapMutator, makeInversionMutator,
    makeInsertionMutator, makeScrambleMutator
  )

  # check validity of produced output for each permutation-based mutator
  for (mutatorGenerator in available.mutators) {
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

test_that("mutation operators working on real-numbered representation create valid offspring", {
  # defs
  n.reps = 5L

  available.mutators = c(makeUniformMutator, makeGaussMutator)

  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator()
    test.seq = runif(5L)
    control = list(par.lower = 0, par.upper = 1)
    for (i in seq(n.reps)) {
      mut.seq = mutate(test.seq, control = control)
      expect_true(all(mut.seq >= 0 && mut.seq <= 1), info = sprintf("Mutator '%s' did not stick to the
        box constraints! Input: (%s), Output: (%s)", getOperatorName(mutate), collapse(test.seq), collapse(mut.seq)))
    }
  }

})
