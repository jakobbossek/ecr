#' Bitplip mutation operator.
#'
#' @param mutator.flip.prob [\code{numeric(1)}]\cr
#'   Probability to flip a single bit. Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @export
makeBitFlipMutator = function(mutator.flip.prob = 0.1) {
  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$mutator.flip.pro, lower = 0.000001, upper = 0.999999, na.ok = FALSE)
  }

  force(mutator.flip.prob)
  defaults = list(mutator.flip.prob = mutator.flip.prob)
  mutatorCheck(defaults)

  mutator = function(setOfIndividuals, control = defaults) {
    n.params = ncol(setOfIndividuals$individuals)
    n = nrow(setOfIndividuals$individuals)

    for (i in seq(n)) {
      do.mutate = runif(n.params) < control$mutator.flip.prob
      setOfIndividuals$individuals[i, do.mutate] = 1 - setOfIndividuals$individuals[i, do.mutate]
    }
    return(setOfIndividuals)
  }

  makeMutator(
    mutator = mutator,
    name = "Bitplip mutator",
    description = "Flips each bit of the allele with a specific probability.",
    supported = "binary",
    defaults = defaults,
    checker = mutatorCheck
  )
}
