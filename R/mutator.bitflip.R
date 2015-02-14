#' Bitplip mutation operator.
#'
#' @param setOfIndividuals [\code{setOfIndividuals}]\cr
#'   Set of individuals.
#' @param control [\code{list}]\cr
#'   Not used.
#' @return [\code{setOfIndividuals}]
#'   Set of individuals.
#' @export
bitflipMutator = function(setOfIndividuals, control) {
  n.params = ncol(setOfIndividuals$individuals)
  n = nrow(setOfIndividuals$individuals)

  for (i in seq(n)) {
    do.mutate = runif(n.params) < control$mutator.flip.prob
    setOfIndividuals$individuals[i, do.mutate] = 1 - setOfIndividuals$individuals[i, do.mutate]
  }
  return(setOfIndividuals)
}

attr(bitflipMutator, "name") = "Bitplip mutator"
attr(bitflipMutator, "description") = "Flips each bit of the allele with a specific probability."
attr(bitflipMutator, "supported") = c("binary")
attr(bitflipMutator, "class") = c("ecr_operator", "ecr_mutator")
#FIXME: hmm, this is problematic! We need to set this to 1/n.params by default
attr(bitflipMutator, "defaults") = list(mutator.flip.prob = 1 / 10)

bitflipMutatorCheck = function(operator.control) {
  assertNumber(operator.control$mutator.flip.pro, lower = 0.000001, upper = 0.999999, na.ok = FALSE)
}

attr(bitflipMutator, "checkFunction") = bitflipMutator
