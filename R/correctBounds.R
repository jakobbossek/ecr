# Corrects the parameters if they are out of bounds.
#
# @param individuals [\code{numeric}]\cr
#   Candidate solution.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}]\cr
#   Parameter set.
# @return [\code{setOfIndividuals}].
correctBounds = function(individual, par.set) {
  lower = getLower(par.set, with.nr = TRUE)
  upper = getUpper(par.set, with.nr = TRUE)

  stopifnot(length(lower) == length(individual))
  individual = pmax(pmin(upper, individual), lower)
  return(individual)
}
