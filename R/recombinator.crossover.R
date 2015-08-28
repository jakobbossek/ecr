#' @title
#'   Generator of the One-point crossover recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function() {
  recombinator = function(inds, control = list(), task) {
    parent1 = inds[[1]]
    parent2 = inds[[2]]
    n = length(parent1)

    if (n == 1L) {
      stopf("Crossover recombinator requires genes to have length > 1.")
    }

    idx = sample(1:(n - 1), size = 1L)

    # at least one allele of each parent should be contained
    child1 = parent1
    child2 = parent2
    child1[(idx + 1L):n] = parent2[(idx + 1L):n]
    child2[1:idx] = parent1[1:idx]
    return(wrapChildren(child1, child2))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical one-point crossover.",
    n.parents = 2L,
    supported = c("float", "binary")
  )
}
