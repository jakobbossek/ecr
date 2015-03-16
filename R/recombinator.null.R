#' The nullRecombinator does not perform any recombination.
#'
#' @return [\code{ecr_recombinator}]
#' @export
makeNullRecombinator = function() {
  recombinator = function(setOfIndividuals, control=list()) {
    child = setOfIndividuals$individuals[1, , drop = FALSE]
    makePopulation(child)
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "NULL recombinator",
    description = "Does not perform any recombination.",
    supported = getAvailableRepresentations(),
    n.parents = 10L
  )
}
