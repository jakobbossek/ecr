#' Swap mutation operator.
#'
#' @return [\code{ecr_mutator}]
#' @export
makeSwapMutator = function() {
  mutator = function(ind, args = list(), control) {
    n.params = length(ind)
    pos = sample(1:n.params, size = 2)
    pos1 = pos[1]
    pos2 = pos[2]
    #catf("Positions: %i, %i", pos1, pos2)
    tmp = ind[pos1]
    ind[pos1] = ind[pos2]
    ind[pos2] = tmp
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Swap mutator",
    description = "Swaps two alleles",
    supported = "permutation",
  )
}
