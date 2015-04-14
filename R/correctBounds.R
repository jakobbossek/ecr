# Corrects the parameters if they are out of bounds.
#
# @param individuals [\code{numeric}]\cr
#   Candidate solution.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}]\cr
#   Parameter set.
# @return [\code{setOfIndividuals}].
correctBounds = function(individuals, par.set, n.params) {
  #FIXME: this was/is damn slow!!! Already improved, but it still sucks, in
  # particular the last if case and the transposition
  lower = getLower(par.set, with.nr = TRUE)
  upper = getUpper(par.set, with.nr = TRUE)

  individuals = lapply(individuals, function(child) {
    pmin(pmax(lower, child), upper)
  })
  return(individuals)
}
