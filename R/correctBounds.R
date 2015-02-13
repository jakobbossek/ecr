# Corrects the parameters if they are out of bounds.
#
# @param individuals [\code{setOfIndividuals}]\cr
#   Set of individuals, for example the entire population.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}]\cr
#   Parameter set.
# @return [\code{setOfIndividuals}].
correctBounds = function(individuals, par.set) {
  for (i in 1:nrow(individuals$individuals)) {
    individuals$individuals[i, ] = repairPoint(par.set, as.list(individuals$individuals[i, ]))
  }
  return(individuals)
}
