#' Swap mutation operator.
#'
#' @return [\code{ecr_mutator}]
#' @export
makeSwapMutator = function() {
  mutator = function(setOfIndividuals, control = list()) {
    n.params = ncol(setOfIndividuals$individuals)
    n = nrow(setOfIndividuals$individuals)

    for (i in seq(n)) {
      pos = sample(1:n.params, size = 2)
      pos1 = pos[1]
      pos2 = pos[2]
      #catf("Positions: %i, %i", pos1, pos2)
      tmp = setOfIndividuals$individuals[i, pos1]
      setOfIndividuals$individuals[i, pos1] = setOfIndividuals$individuals[i, pos2]
      setOfIndividuals$individuals[i, pos2] = tmp
    }
    return(setOfIndividuals)
  }

  makeMutator(
    mutator = mutator,
    name = "Swap mutator",
    description = "Swaps two alleles",
    supported = "permutation",
  )
}
