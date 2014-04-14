# Checks whether the EA run should terminate.
#
# @param current.iter [\code{integer(1)}]\cr
#   Current iteration of the algorithm.
# @param max.iter [\code{integer(1)}]\cr
#   Maximum number of iterations.
# @return [\code{logical(1)}]
isTerminiationCriterionFullfilled = function(current.iter, max.iter) {
  current.iter > max.iter
}
