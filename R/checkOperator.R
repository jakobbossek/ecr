# @title
#   Checks operator attributes for validity
#
# @param operator
#   Operator to check.
# @return Nothing
checkOperator = function(operator) {
  assertClass(operator, "ecr_operator")
  assertCharacter(attr(operator, "name"), len = 1L, any.missing = FALSE)
  assertCharacter(attr(operator, "description"), len = 1L, any.missing = FALSE)
  assertCharacter(attr(operator, "supported"), min.len = 1L, any.missing = FALSE)
  assertList(attr(operator, "defaults"), any.missing = FALSE)
}

checkMutator = function(mutator) {
  checkOperator(mutator)
  assertClass(mutator, "ecr_mutator")
}

checkRecombinator = function(recombinator) {
  checkOperator(recombinator)
  assertClass(recombinator, "ecr_recombinator")
  assertInteger(attr(recombinator, "n.parents"), len = 1L, lower = 2L, any.missing = FALSE)
  assertList(attr(recombinator, "defaults"), any.missing = FALSE)
}
