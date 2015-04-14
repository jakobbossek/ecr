#' Generator of the crossover recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function() {
  recombinator = function(inds, control = list()) {
    parent1 = inds[[1]]
    parent2 = inds[[2]]
    n = length(parent1)
    # at least one allele of each parent should be contained
    idx = sample(0:n, size = 1L)
    child = parent1
    child[idx:n] = parent2[idx:n]
    return(child)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical crossover",
    n.parents = 2L,
    supported = c("float", "binary")
  )
}
