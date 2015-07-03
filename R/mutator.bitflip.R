#' Bitplip mutation operator.
#'
#' @param mutator.flip.prob [\code{numeric(1)}]\cr
#'   Probability to flip a single bit. Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @export
makeBitFlipMutator = function(mutator.flip.prob = 0.1) {
  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$mutator.flip.prob, lower = 0.000001, upper = 0.999999, na.ok = FALSE)
  }

  force(mutator.flip.prob)
  defaults = list(mutator.flip.prob = mutator.flip.prob)
  mutatorCheck(defaults)

  mutator = function(ind, args = defaults, control) {
    mutateSubGene = function(sub.gen, prob) {
      do.mutate = runif(length(sub.gen)) < prob
      sub.gen[do.mutate] = 1 - sub.gen[do.mutate]
      sub.gen
    }
      
    ind = lapply(ind, mutateSubGene, prob = args$mutator.flip.prob)

    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Bitflip mutator",
    description = "Flips each bit of the allele with a specific probability.",
    supported = "binary",
    defaults = defaults,
    checker = mutatorCheck
  )
}
