# Checks whether the EA run should terminate.
#
# @param current.iter [\code{integer(1)}]\cr
#   Current iteration of the algorithm.
# @param max.iter [\code{integer(1)}]\cr
#   Maximum number of iterations.
# @return [\code{logical(1)}]
isTerminiationCriterionFullfilled = function(current.iter, max.iter) {
  #FIXME: max.iter should be wrapped in something like a 'esooControl' object ...
  #FIXME: ... as should other paramters which are important for the termination
  #       like the maximum running time, the longest allowed sequence of iterations
  #       without finding a better individual and so on.
  current.iter > max.iter
}
