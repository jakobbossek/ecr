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
    assertNumber(operator.control$p, lower = 0.000001, upper = 0.999999, na.ok = FALSE)
  }

  force(p)
  defaults = list(p = p)
  mutatorCheck(defaults)

  mutator = function(ind, args = defaults, control, task) {
    n.params = length(ind)
    do.mutate = runif(n.params) < args$p
    ind[do.mutate] = 1 - ind[do.mutate]
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
