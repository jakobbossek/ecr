#' Bitplip mutation operator.
#'
#' @param mutator.flip.prob [\code{numeric(1)}]\cr
#'   Probability to flip a single bit. Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @export
makeBitFlipMutator = function(mutator.flip.prob = 0.1) {
  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$mutator.flip.prob, lower = 0, upper = 1, na.ok = FALSE)
  }

  force(mutator.flip.prob)
  defaults = list(mutator.flip.prob = mutator.flip.prob)
  mutatorCheck(defaults)

  mutator = function(ind, args = defaults, control) {
    mutateGene = function(gene, prob) {
      do.mutate = runif(length(gene)) < prob
      gene[do.mutate] = 1 - gene[do.mutate]
      gene
    }
    
    if (getParamNr(control$par.set) == 1L) {
      ind = mutateGene(ind, args$mutator.flip.prob)
    } else {
      ind = lapply(ind, mutateGene, prob = args$mutator.flip.prob)
    }
    
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
