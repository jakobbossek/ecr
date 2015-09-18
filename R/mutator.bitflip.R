#' @title
#'   Generator of a simple bitplip mutation operator.
#'
#' @description
#'   This operator works only on binary representation and flips each bit
#'   with a given probability.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability to flip a single bit. Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
makeBitFlipMutator = function(p = 0.1) {
  mutatorCheck = function(operator.control) {
    assertNumber(operator.control$p, lower = 0, upper = 1)
  }

  force(p)
  defaults = list(p = p)
  mutatorCheck(defaults)

  mutator = function(ind, args = defaults, control, task) {
    n.params = length(ind)
    do.mutate = runif(n.params) < args$p
    ind[do.mutate] = 1 - ind[do.mutate]
    mutateGene = function(gene, prob) {
      do.mutate = runif(length(gene)) < prob
      gene[do.mutate] = 1 - gene[do.mutate]
      gene
    }

    if (getParamNr(control$par.set) == 1L) {
      ind = mutateGene(ind, args$p)
    } else {
      ind = lapply(ind, mutateGene, prob = args$p)
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
