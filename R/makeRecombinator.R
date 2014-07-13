# Helper function which constructs a recombinator, i. e., a recombination operator.
#
# @param recombinator [\code{function}]\cr
#   Actual mutation operator.
# @param name [\code{character(1)}]\cr
#   Name of the recombinator.
# @param supported [\code{character}]\cr
#   Vector of strings/names of supported parameter representations. For example
#   'permutation', 'float', 'binary'.
# @param n.parents [\code{integer(1)}]\cr
#   Number of parents supported.
# @return [\code{ecr_recombinator}]
#   Recombinator object.
makeRecombinator = function(
  recombinator, name,
  supported = getAvailableRepresentations(),
  n.parents = 2L) {
  assertInteger(n.parents, len = 1L, lower = 2L, any.missing = FALSE)
  recombinator = makeOperator(recombinator, name, supported)
  attr(recombinator, "n.parents") = n.parents
  recombinator = addClasses(recombinator, c("ecr_recombinator"))
  return(recombinator)
}
