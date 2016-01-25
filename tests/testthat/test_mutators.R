context("mutation operators (mutators)")

test_that("mutation operators working on permutation genes create valid offspring", {
  # defs
  s = 1:10L
  n.reps = 5L

  # gather all mutators for permutation representation
  available.mutators = c(
    setupSwapMutator, setupInversionMutator,
    setupInsertionMutator, setupScrambleMutator
  )

  # check validity of produced output for each permutation-based mutator
  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator() # no mutation control parameters to check here
    expect_true(isEcrOperator(mutate))
    expect_output(print(mutate), regexp = "Name")
    test.seq = sample(s, 10L, replace = FALSE)
    task = list(par.set = makeNumericParamSet(len = 10L, id = "c", lower = 1L, upper = 10L))
    for (i in seq(n.reps)) {
      perm.seq = mutate(test.seq, task, control = NULL)
      expect_true(setequal(test.seq, perm.seq), info = sprintf("Mutator '%s' did not produce a valid
        permutation! Input: (%s), Output: (%s)", getOperatorName(mutate),
        collapse(test.seq), collapse(perm.seq)))
    }
  }
})

test_that("mutation operators working on real-numbered representation create valid offspring", {
  # defs
  n.reps = 5L

  available.mutators = c(setupUniformMutator, setupGaussMutator)

  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator()
    test.seq = runif(5L)
    task = list(par.set = makeNumericParamSet("x", len = 5L, lower = 0, upper = 1))
    for (i in seq(n.reps)) {
      mut.seq = mutate(test.seq, task, control = NULL)
      expect_true(all(mut.seq >= 0 && mut.seq <= 1), info = sprintf("Mutator '%s' did not stick to the
        box constraints! Input: (%s), Output: (%s)", getOperatorName(mutate), collapse(test.seq), collapse(mut.seq)))
    }
  }

})
