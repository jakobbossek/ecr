# Helper function for recombination of individuals.
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
recombinate = function(individuals, type = "intermediate", params = list(weight = 0.5)) {
  #FIXME: weight not considered until now
  #FIXME: add more recombination operators! How to effectively determine if the selected
  #       type is "compatible" with the representation of the individuals.
  child = apply(individuals$population, 2, sum) / 2
  makePopulation(t(as.matrix(child)))
}
