#' Generator of the indermediate recombination operator.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeIntermediateRecombinator = function() {
  recombinator = function(setOfIndividuals, control = list()) {
    inds = setOfIndividuals$individuals
    n = length(inds[[1]])
    child = rep(0, n)
    for (i in 1:length(inds)) {
      child = child + inds[[i]]
    }
    child = child / length(inds)
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
