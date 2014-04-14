# Checks whether the EA run should terminate.
#
# @param current.iter [\code{integer(1)}]\cr
#   Current iteration of the algorithm.
# @param max.iter [\code{integer(1)}]\cr
#   Maximum number of iterations.
# @param global.optimum [\code{numeric}]\cr
#   Parameter values of global optimum or NA.
# @param best [\code{esooIndividual}]\cr
#   Current best individual.
# @param termination.eps [\code{numeric(1)}]\cr
#   Maximal gap between best individual so far and the global optimum.
# @return [\code{logical(1)}]
isTerminiationCriterionFullfilled = function(current.iter, max.iter, global.optimum, best, termination.eps) {
  should.terminate = (current.iter > max.iter)
  #FIXME: this should be made better / more effective. Until now it is ugly as sin. Particularly the number
  #       of parameters.
  if (!any(is.na(global.optimum))) {
    best.param = best$individual
    gap = sqrt(sum((best.param - global.optimum)^2))
    should.terminate = (should.terminate || (gap < termination.eps))
  }
  return(should.terminate)
}
