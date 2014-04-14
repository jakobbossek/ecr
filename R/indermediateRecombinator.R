# Generator of the indermediate recombination operator.
#
# @param individuals [\code{setOfIndividuals}]\cr
#   Set of individuals.
# @param type [\code{character(1)}]\cr
#   String indicating the type of recombination used. Until now only the default value
#   intermediate is implemented.
# @param params [\code{list}]\cr
#   Further arguments for the recombination procedure (for example weights for intermediate recombination).
# @return [\code{setOfIndividuals}]
#   Recombined offspring.
makeIntermediateRecombinator = function() {
  # Helper function for recombination of individuals.
  #
  # @param setOfIndividuals [\code{setOfIndividuals}]\cr
  #   Set of individuals.
  # @param control [\code{list}]\cr
  #   Control object.
  # @return [\code{setOfIndividuals}]
  #   Recombined offspring.
  intermediateRecombinator = function(setOfIndividuals, control=list()) {
    child = apply(setOfIndividuals$population, 2, sum) / 2
    makePopulation(t(as.matrix(child)))
  }

  makeRecombinator(
    recombinator = intermediateRecombinator,
    name = "Intermediate recombinator",
    supported = c("float"),
    n.parents = 10L)
}
