# @title Checks operator attributes for validity
#
# @param operator [\code{ecr_operator}]\cr
#   Operator to check.
checkOperator = function(operator) {
  assertClass(operator, "ecr_operator")
  assertCharacter(attr(operator, "name"), len = 1L, any.missing = FALSE)
  assertCharacter(attr(operator, "description"), len = 1L, any.missing = FALSE)
  assertCharacter(attr(operator, "supported"), min.len = 1L, any.missing = FALSE)
  assertList(attr(operator, "params"), any.missing = FALSE)
}

# @title Checks mutator attributes for validity
#
# @param operator [\code{ecr_operator}]\cr
#   Operator to check.
checkMutator = function(mutator) {
  checkOperator(mutator)
  assertClass(mutator, "ecr_mutator")
}

# @title Checks recombinator attributes for validity
#
# @param operator [\code{ecr_operator}]\cr
#   Operator to check.
checkRecombinator = function(recombinator) {
  checkOperator(recombinator)
  assertClass(recombinator, "ecr_recombinator")
  assertInteger(attr(recombinator, "n.parents"), len = 1L, lower = 2L, any.missing = FALSE)
  assertFlag(attr(recombinator, "multiple.children"))
}
