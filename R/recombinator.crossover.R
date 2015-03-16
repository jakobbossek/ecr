#' Generator of the crossover recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeCrossoverRecombinator = function() {
  recombinator = function(setOfIndividuals, control = list()) {
    parents = setOfIndividuals$individuals
    parent1 = parents[1, ]
    parent2 = parents[2, ]
    n = length(parent1)
    # at least one allele of each parent should be contained
    idx = sample(2:(n - 1), size = 1L)
    child = parent1
    child[idx:n] = parent2[idx:n]
    child = matrix(child, nrow = 1L)
    makePopulation(child)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical crossover",
    n.parents = 2L,
    supported = c("float", "binary")
  )
}
