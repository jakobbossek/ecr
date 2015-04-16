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

  mutator = function(ind, args = defaults, control) {
    n.params = length(ind)
    do.mutate = runif(n.params) < args$mutator.flip.prob
    ind[do.mutate] = 1 - ind[do.mutate]
    return(ind)
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
