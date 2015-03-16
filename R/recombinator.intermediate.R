#' Generator of the indermediate recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeIntermediateRecombinator = function() {
  recombinator = function(setOfIndividuals, control = list()) {
    child = matrix(colSums(setOfIndividuals$individuals) / 2, nrow = 1L)
    makePopulation(child)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Intermediate recombinator",
    description = "Performs intermediate recombination.",
    supported = "float",
    n.parents = 10L
  )
}
