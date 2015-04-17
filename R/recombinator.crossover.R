#' Generator of the One-point crossover recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function() {
  recombinator = function(inds, control = list()) {
    parent1 = inds[[1]]
    parent2 = inds[[2]]
    n = length(parent1)
    #FIXME: we have to make sure, that the gene has length > 1. This should not
    # be the case in pratice use, but it causes errors
    idx = sample(1:(n - 1), size = 1L)
    # at least one allele of each parent should be contained
    child1 = parent1
    child2 = parent2
    child1[(idx + 1L):n] = parent2[(idx + 1L):n]
    child2[1:idx] = parent1[1:idx]
    #FIXME: here we just return one offspring for now
    return(child1)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical one-point crossover.",
    n.parents = 2L,
    supported = c("float", "binary")
  )
}
